## ---- init

library(ggplot2)
library(hrbrthemes)
library(scales)
library(tidyverse)
library(funFEM)
library(tidyverse)
library(ggmap)
library(FactoMineR)
library(patchwork)
library(stats)
library(factoextra)
library(aricode)
library(kableExtra)
library(stringr)
library(mclust)

set.seed(0)

default_theme <- theme_ipsum(base_family = "") + theme(
  axis.title.x = element_text(hjust = 0.5),
  axis.title.y = element_text(hjust = 0.5),
  plot.margin = margin(
    t = 0.5,
    r = 0.5, b = 0.5, l = 0.5, "cm"
  ),
  legend.position = "bottom"
)

theme_set(default_theme)

knitr::opts_chunk$set(
  dev = "tikz", message = F, fig.align = "center", warning = F
)
options(tikzDefaultEngine = "luatex")

## ---- read-data

data(velib)

# select exactly 7 days of data (we remove the first 13 dates)
data <- as.matrix(velib$data[, -c(1:13)])
velib_id <- as.integer(rownames(data))
colnames(data) <- seq_len(ncol(data))
rownames(data) <- velib$names

day_of_week <- weekdays(as.Date(1:7, origin = "1950-01-01"))

df <- data.frame(data) %>%
  mutate(id = velib_id) %>%
  pivot_longer(
    -id,
    names_to = "hour",
    names_transform = list(hour = function(x) as.integer(substring(x, 2))),
    values_to = "loading"
  ) %>%
  mutate(
    day = factor(
      day_of_week[(hour - 1) %/% 24 + 1],
      ordered = T,
      levels = day_of_week
    ),
    hour_in_day = as.integer((hour - 1) %% 24)
  )

df_station <- data.frame(
  id = velib_id, name = velib$name, hill = as.logical(velib$bonus)
)
df_station[c("long", "lat")] <- velib$position

rownames(data) <- seq_len(nrow(data))

time_breaks <- 1 + 24 * (0:6)

## ---- first-stations

g_first_stations <- df[df$id %in% velib_id[1:4], ] %>%
  inner_join(df_station, by = "id") %>%
  ggplot(aes(x = hour, y = loading, group = name)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  geom_vline(
    xintercept = time_breaks, colour = alpha("grey", 0.9)
  ) +
  geom_line(colour = "blue", size = 0.2) +
  facet_wrap(~name, ncol = 2) +
  theme(strip.text = element_text(size = 10))

## ---- boxplot-df

g_boxplot_df <- df %>%
  ggplot(aes(x = factor(hour), y = loading)) +
  geom_boxplot() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  geom_vline(
    xintercept = time_breaks, colour = alpha("blue", 0.9), linetype = "dotted"
  )

## ---- avg-hourly

g_avg_hourly <- df %>%
  group_by(hour, day, hour_in_day) %>%
  summarise(mean = mean(loading), .groups = "rowwise") %>%
  ggplot(aes(x = hour_in_day, y = mean, color = day)) +
  geom_line() +
  ylim(c(0.1, NA)) +
  scale_x_discrete(limits = 0:23, expand = c(0, 0)) +
  geom_point() +
  xlab("Hour in a day") +
  ylab(NULL) +
  scale_color_discrete(name = "Day of week")

## ---- map-avg-hour

df_map_avg_hour <- df %>%
  filter(hour_in_day %in% c(6, 12, 23)) %>%
  group_by(id, hour_in_day) %>%
  summarise(mean = mean(loading), .groups = "rowwise") %>%
  bind_rows(
    df %>%
      group_by(id) %>%
      summarise(mean = mean(loading), .groups = "rowwise") %>%
      mutate(hour_in_day = 24)
  ) %>%
  inner_join(df_station, by = "id")

g_map_avg_hour <- qmplot(
  long, lat,
  color = mean, group = hour_in_day,
  data = df_map_avg_hour,
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "bottomright"
) + scale_color_continuous(name = "Loading") +
  facet_wrap(~hour_in_day,
    ncol = 2,
    labeller = labeller(
      hour_in_day = c(
        `6` = 6, `12` = 12, `23` = 23, `24` = "Average weekly"
      )
    )
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.spacing = unit(0.5, "lines")
  )

## ---- map-hill

g_map_hill <- qmplot(
  long, lat,
  color = hill,
  data = df_station,
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "bottom"
) + theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
)

## ---- boxplot-hill

g_boxplot_hill <- df %>%
  inner_join(df_station, by = "id") %>%
  ggplot(aes(x = factor(hour), y = loading)) +
  geom_boxplot(outlier.stroke = 0, outlier.shape = ".") +
  facet_grid(hill ~ ., labeller = labeller(
    hill = c(`FALSE` = "Non-hill", `TRUE` = "Hill")
  )) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  geom_vline(
    xintercept = time_breaks, colour = alpha("blue", 0.9), linetype = "dotted"
  )

## ---- avg-hill

g_avg_hill <- df %>%
  inner_join(df_station, by = "id") %>%
  group_by(hour, day, hour_in_day, hill) %>%
  summarise(mean = mean(loading), .groups = "rowwise") %>%
  ggplot(aes(x = hour_in_day, y = mean, color = day)) +
  geom_line() +
  ylim(c(0, NA)) +
  facet_grid(hill ~ ., labeller = labeller(
    hill = c(`FALSE` = "Non-hill", `TRUE` = "Hill")
  )) +
  scale_x_discrete(limits = 0:23, expand = c(0, 0)) +
  geom_point() +
  xlab("Hour in a day") +
  ylab(NULL) +
  scale_color_discrete(name = "Day of week")

## ---- pca

pca <- PCA(data, scale.unit = T, graph = F)
pca_eig_df <- data.frame(pca$eig[1:10, ]) %>% rownames_to_column("comp")
pca_ncp <- which(pca_eig_df[, 4] >= 75)[1]
pca_data <- pca$ind$coord[, 1:pca_ncp]

## ---- pca-inertia-bar

g_inertia_bar <- pca_eig_df %>% ggplot(aes(
  comp, percentage.of.variance
)) +
  geom_col(fill = "#454759") +
  scale_x_discrete(
    guide = guide_axis(check.overlap = T),
    limits = pca_eig_df$comp
  )

## ---- pca-inertia-percent

g_inertia_percent <- pca_eig_df %>% ggplot(aes(
  1:10, cumulative.percentage.of.variance,
  group = 1
)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(
    guide = guide_axis(check.overlap = T),
    limits = 1:10
  ) +
  geom_hline(yintercept = 75, linetype = "longdash", color = "red") +
  xlab("Component") +
  ylab("Cumulative percentage of variance")

## ---- pca-var

g_pca_var <- fviz_pca_var(
  pca,
  geom = "arrow", alpha.var = 0.5
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- pca-graph

g_pca <- g_inertia_percent + g_pca_var + plot_layout(widths = c(1, 2))

## ---- pca-svd

g_pca_svd <- data.frame(pca$svd$V) %>%
  rownames_to_column("id") %>%
  mutate(id = as.integer(id)) %>%
  pivot_longer(-id,
    names_to = "comp",
    names_transform = list(comp = function(x) as.integer(substring(x, 2)))
  ) %>%
  ggplot(aes(id, value, group = comp)) +
  geom_line() +
  xlab(NULL) +
  ylab("Coefficient") +
  scale_x_discrete(guide = guide_axis(check.overlap = T)) +
  facet_grid(comp ~ .) +
  geom_vline(
    xintercept = time_breaks, colour = alpha("blue", 0.9), linetype = "dotted"
  )

## ---- nbc

hc_k <- 3
km_k <- 3

## ---- hc-raw

hc_raw <- hcut(data, hc_k, hc_method = "ward.D2")
hc_raw_height <- rev(hc_raw$height)

## ---- hc-raw-height

g_hc_raw_height <- data.frame(height = hc_raw_height[1:10]) %>%
  rownames_to_column("id") %>%
  mutate(id = as.integer(id)) %>%
  ggplot(aes(id, height), stroke = 0) +
  scale_x_discrete(limits = 1:10) +
  ylim(c(0, NA)) +
  annotate(
    "segment",
    x = 1, y = hc_raw_height[1], xend = 1, yend = hc_raw_height[2],
    arrow = arrow(length = unit(0.02, "npc"), ends = "both"),
    linetype = "longdash"
  ) +
  annotate(
    "segment",
    x = 2, y = hc_raw_height[2], xend = 2, yend = hc_raw_height[3],
    arrow = arrow(length = unit(0.02, "npc"), ends = "both"),
    linetype = "longdash"
  ) +
  annotate(
    "text",
    x = 2, y = (hc_raw_height[1] + hc_raw_height[2]) / 2,
    label = round(hc_raw_height[1] - hc_raw_height[2], 2)
  ) +
  annotate(
    "text",
    x = 3, y = (hc_raw_height[2] + hc_raw_height[3]) / 2,
    label = round(hc_raw_height[2] - hc_raw_height[3], 2)
  ) +
  geom_point() +
  xlab("Number of classes") +
  ylab("Height")

## ---- hc-raw-dendro

g_hc_raw_dendro <- fviz_dend(
  hc_raw,
  k = hc_k,
  color_labels_by_k = T,
  show_labels = F,
  labels_track_height = 0,
  ggtheme = theme_void(),
  main = "",
) + scale_y_continuous(labels = NULL, breaks = NULL)

## ---- hc-raw-graph

g_hc_raw <- g_hc_raw_height + g_hc_raw_dendro

## ---- hc-raw-cmap

g_hc_raw_cmap <- qmplot(
  long, lat,
  color = cluster,
  data = data.frame(
    id = velib_id, cluster = factor(hc_raw$cluster)
  ) %>% inner_join(df_station, by = "id"),
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "none",
  padding = 0
) + default_theme + theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none"
)

## ---- hc-raw-cpos

g_hc_raw_cpos <- fviz_pca_ind(
  pca,
  geom = "point",
  habillage = factor(hc_raw$cluster),
  pointsize = 1,
  addEllipses = T
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- hc-raw-cgraph

g_hc_raw_c <- g_hc_raw_cpos +
  g_hc_raw_cmap +
  plot_layout(widths = c(1, 1), guide = "collect")

## ---- hc-pca

hc_pca <- hcut(
  pca_data, hc_k,
  hc_method = "ward.D2"
)

hc_pca$cluster <- apply(
  table(hc_raw$cluster, hc_pca$cluster), 2, which.max
)[hc_pca$cluster]

## ---- hc-pca-cmap

g_hc_pca_cmap <- qmplot(
  long, lat,
  color = cluster,
  data = data.frame(
    id = velib_id, cluster = factor(hc_pca$cluster)
  ) %>% inner_join(df_station, by = "id"),
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "none",
  padding = 0
) + default_theme + theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none"
)

## ---- hc-pca-cpos

g_hc_pca_cpos <- fviz_pca_ind(
  pca,
  geom = "point",
  habillage = factor(hc_pca$cluster),
  pointsize = 1,
  addEllipses = T
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- hc-pca-cgraph

g_hc_pca_c <- g_hc_pca_cpos +
  g_hc_pca_cmap +
  plot_layout(widths = c(1, 1), guide = "collect")

## ---- hc-raw-pca-nmi

hc_raw_pca_nmi <- NMI(hc_raw$cluster, hc_pca$cluster)

## ---- km-raw

km_raw <- kmeans(data, centers = km_k)

km_raw$cluster <- apply(
  table(hc_raw$cluster, km_raw$cluster), 2, which.max
)[km_raw$cluster]

## ---- km-raw-cpos

g_km_raw_cpos <- fviz_pca_ind(
  pca,
  geom = "point",
  habillage = factor(km_raw$cluster),
  pointsize = 1,
  addEllipses = T
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- km-raw-cmap

g_km_raw_cmap <- qmplot(
  long, lat,
  color = cluster,
  data = data.frame(
    id = velib_id, cluster = factor(km_raw$cluster)
  ) %>% inner_join(df_station, by = "id"),
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "none",
  padding = 0
) + default_theme + theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none"
)

## ---- km-raw-cgraph

g_km_raw_c <-
  g_km_raw_cpos +
  g_km_raw_cmap +
  plot_layout(widths = c(1, 1), guide = "collect")

## ---- km-pca

km_pca <- kmeans(pca_data, centers = km_k)

km_pca$cluster <- apply(
  table(hc_raw$cluster, km_pca$cluster), 2, which.max
)[km_pca$cluster]

## ---- km-pca-cpos

g_km_pca_cpos <- fviz_pca_ind(
  pca,
  geom = "point",
  habillage = factor(km_pca$cluster),
  pointsize = 1,
  addEllipses = T
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- km-pca-cmap

g_km_pca_cmap <- qmplot(
  long, lat,
  color = cluster,
  data = data.frame(
    id = velib_id, cluster = factor(km_pca$cluster)
  ) %>% inner_join(df_station, by = "id"),
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "none",
  padding = 0
) + default_theme + theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none"
)

## ---- km-pca-cgraph

g_km_pca_c <- g_km_pca_cpos +
  g_km_pca_cmap +
  plot_layout(widths = c(1, 1), guide = "collect")

## ---- km-raw-pca-nmi

km_raw_pca_nmi <- NMI(km_raw$cluster, km_pca$cluster)

## ---- mc

mc <- Mclust(pca_data)

## ----- mc-bic

g_mc_bic <- fviz_mclust_bic(
  mc,
  ggtheme = default_theme,
  main = "",
  xlab = "",
  ylab = "",
  legend = "bottom"
)

## ----- mc-cpos

g_mc_cpos <- fviz_pca_ind(
  pca,
  geom = "point",
  habillage = factor(mc$classification),
  pointsize = 1,
  addEllipses = T
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- mc-graph

g_mc_graph <- g_mc_bic + g_mc_cpos

## ---- df-mca

df_mca <- df %>%
  mutate(isday = hour_in_day %in% 6:19) %>%
  group_by(isday, day, id) %>%
  summarise(mean = mean(loading), .groups = "rowwise") %>%
  mutate(
    loading =
      cut(mean, breaks = c(-Inf, 1:3) / 3, labels = letters[1:3]),
    colname = paste0(
      ifelse(isday, "day", "night"),
      as.numeric(day)
    )
  ) %>%
  pivot_wider(id_cols = id, names_from = colname, values_from = loading) %>%
  inner_join(df_station, by = "id") %>%
  dplyr::select(c(17, 9:15, 2:8)) %>%
  mutate(hill = factor(ifelse(hill, "hill", "nohill")))

## ---- df-mca-table

head(df_mca) %>%
  kbl(
    caption = paste("New dataset with qualitative data")
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  )

## ---- mca

mca <- MCA(df_mca, graph = F)

## ---- mca-ind

g_mca_ind <- plot(
  mca,
  choix = "ind", invisible = "ind",
  habillage = "quali", graph.type = "ggplot",
  title = ""
) + xlab(
  paste0("Dim 1 ($", round(mca$eig[1, 2], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(mca$eig[2, 2], 2), "\\%$)")
) + labs(title = NULL) + default_theme

g_mca_ind_build <- ggplot_build(g_mca_ind)
g_mca_ind_build$data[[4]]$label[-c(1, 2)] <-
  str_replace(
    g_mca_ind_build$data[[4]]$label[-c(1, 2)],
    "_",
    "$\\\\texttt{\\\\textunderscore}$"
  )
g_mca_ind <- ggpubr::as_ggplot(ggplot_gtable(g_mca_ind_build))

## ---- mca-ind-hill

g_mca_ind_hill <- plot(
  mca,
  label = "no",
  graph.type = "ggplot"
) + xlab(
  paste0("Dim 1 ($", round(mca$eig[1, 2], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(mca$eig[2, 2], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- km-mca

km_mca <- kmeans(mca$ind$coord[, 1:pca_ncp], centers = km_k)

## ---- km-mca-cpos

g_km_mca_cpos <- fviz_mca_ind(
  mca,
  geom = "point",
  habillage = factor(km_mca$cluster),
  pointsize = 1,
  addEllipses = T
) + xlab(
  paste0("Dim 1 ($", round(pca_eig_df[1, 3], 2), "\\%$)")
) + ylab(
  paste0("Dim 2 ($", round(pca_eig_df[2, 3], 2), "\\%$)")
) + labs(title = NULL) + default_theme

## ---- km-mca-cmap

g_km_mca_cmap <- qmplot(
  long, lat,
  color = cluster,
  data = data.frame(
    id = velib_id, cluster = factor(km_mca$cluster)
  ) %>% inner_join(df_station, by = "id"),
  source = "stamen",
  maptype = "terrain-lines",
  stroke = 0,
  legend = "none",
  padding = 0
) + default_theme + theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none"
)

## ---- km-mca-cgraph

g_km_mca_c <- g_km_mca_cpos +
  g_km_mca_cmap +
  plot_layout(widths = c(1, 1), guide = "collect")

## ---- boxplot-km-mca

g_boxplot_km_mca <- df %>%
  inner_join(data.frame(
    id = velib_id, cluster = km_mca$cluster
  ), by = "id") %>%
  ggplot(aes(x = factor(hour), y = loading)) +
  geom_boxplot(outlier.stroke = 0, outlier.shape = ".") +
  facet_wrap(~cluster, ncol = 2) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  geom_vline(
    xintercept = time_breaks, colour = alpha("blue", 0.9), linetype = "dotted"
  )