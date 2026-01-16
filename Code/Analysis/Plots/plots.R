
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load 2024 WRLU fields
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  filter(year == 2024) |> 
  st_filter(gsl_basin, .predicate = st_intersects)

# Load field-level annual depletions
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# Filter depletion data to GSL Basin fields
depletion_annual = depletion_annual |> 
  filter(year > 2017, id %in% fields$id, crop != "Fallow/Idle")

# ==== PREP ====================================================================

by_crop = depletion_annual |> 
  group_by(crop, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(crop) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop"
  ) |> 
  filter(!is.na(crop)) |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 10)

plot_data = depletion_annual |> 
  filter(crop %in% by_crop$crop) |> 
  mutate(crop = factor(crop, levels = by_crop$crop)) |> 
  group_by(crop) |> 
  mutate(
    median_depletion_ft = median(depletion_ft, na.rm = TRUE),
    mean_depletion_ft = mean(depletion_ft, na.rm = TRUE),
  ) |> 
  ungroup() |> 
  mutate(median_diversion = depletion_ft - median_depletion_ft)

plot_data1 = depletion_annual |> 
  filter(crop %in% by_crop$crop[1:5]) |> 
  mutate(crop = factor(crop, levels = by_crop$crop)) |> 
  group_by(crop) |> 
  mutate(median_depletion_ft = median(depletion_ft, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(median_diversion = depletion_ft - median_depletion_ft)

plot_data2 = depletion_annual |> 
  filter(crop %in% by_crop$crop[6:10]) |> 
  mutate(crop = factor(crop, levels = by_crop$crop)) |> 
  group_by(crop) |> 
  mutate(median_depletion_ft = median(depletion_ft, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(median_diversion = depletion_ft - median_depletion_ft)
 
label_data = plot_data |> 
  group_by(crop) |> 
  summarize(
    q1 = quantile(depletion_ft, 0.25, na.rm = TRUE),
    q3 = quantile(depletion_ft, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    whisker_min = min(depletion_ft[depletion_ft >= q1 - 1.5 * iqr], na.rm = TRUE),
    whisker_max = max(depletion_ft[depletion_ft <= q3 + 1.5 * iqr], na.rm = TRUE),
    median = median(depletion_ft, na.rm = TRUE),
    .groups = "drop"
  )

# ==== BOX AND WHISKER =========================================================

depletion_boxplot = ggplot() +
  geom_boxplot(
    data = plot_data,
    aes(x = crop, y = depletion_ft),
    outliers = FALSE,
    fill = "lightblue",
    width = 0.45
  ) +
  geom_text(
    data = label_data,
    aes(x = crop, y = median, label = round(median, 2)),
    vjust = -0.6,
    size = 4.5,
  ) +
  geom_label(
    data = label_data,
    aes(x = crop, y = whisker_min, label = round(whisker_min, 2)),
    vjust = 0.5,
    size = 4.5,
    border.color = "black",
    linewidth = 0.4
  ) +
  geom_label(
    data = label_data,
    aes(x = crop, y = whisker_max, label = round(whisker_max, 2)),
    vjust = 0,
    size = 4.5,
    border.color = "black",
    linewidth = 0.4
  ) +
  labs(
    y = "Depletion (AFA)",
    x = NULL,
    title = "Dispersion of Field-Level Depletion Depth, Top Ten Crops"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(color = "black", size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  )
depletion_boxplot

# Save plot
ggsave(
  "Figures/Plots/depletion_boxplot.png", 
  plot = depletion_boxplot,
  dpi = 500,
  width = 13,
  height = 7,
  bg = "white"
)

# ==== DEPLETION DEPTH HISTOGRAM ===============================================

hist_labels = plot_data |> 
  group_by(crop) |> 
  summarize(
    median_depletion_ft = median(depletion_ft, na.rm = TRUE),
    mean_depletion_ft = mean(depletion_ft, na.rm = TRUE),
    .groups = "drop"
  )

depletion_hist = ggplot() +
  geom_histogram(
    data = plot_data,
    aes(x = depletion_ft),
    fill = "deepskyblue3",
    bins = 150
  ) +
  geom_vline(
    data = plot_data,
    aes(xintercept = median_depletion_ft),
    linetype = "dashed"
  ) + 
  geom_label(
    data = hist_labels,
    aes(
      x = median_depletion_ft,
      y = Inf,
      label = round(median_depletion_ft, 2)
    ),
    vjust = 1.2,
    size = 4,
    label.size = 0.3
  ) +
  facet_wrap(~crop, scales = "free", ncol = 2, axes = "all") +
  geom_text(
    data = subset(plot_data, crop == levels(plot_data$crop)[1]),
    aes(
      x = median_depletion_ft,
      y = Inf,
      label = "← Median",
      fontface = "plain"
    ),
    vjust = 4.5,
    hjust = -0.1,
    size = 4.5,
    inherit.aes = FALSE
  ) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(
    title = "Dispersion of Field-Level Depletion Depth, Top Ten Crops",
    x = "Depletion (AFA)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(size = 15, hjust = 0.5, color = "black"),
    strip.text = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black")
  )
depletion_hist

# Save plot
ggsave(
  "Figures/Plots/depletion_hist.png", 
  plot = depletion_hist,
  dpi = 500,
  width = 13.5,
  height = 7.5,
  bg = "white"
)





plot1 = ggplot() +
  geom_histogram(
    data = plot_data1,
    aes(x = depletion_ft),
    bins = 100
  ) +
  geom_vline(
    data = plot_data1,
    aes(xintercept = median_depletion_ft)
  ) + 
  facet_wrap(~crop, scales = "fixed", ncol = 1, axes = "all") +
  coord_cartesian(xlim = c(0, 4)) +
  theme_minimal()
plot1

plot2 = ggplot() +
  geom_histogram(
    data = plot_data2,
    aes(x = depletion_ft),
    bins = 100
  ) +
  geom_vline(
    data = plot_data2,
    aes(xintercept = median_depletion_ft)
  ) + 
  facet_wrap(~crop, scales = "fixed", ncol = 1, axes = "all") +
  coord_cartesian(xlim = c(0, 4)) +
  theme_minimal() +
  theme(
    
  )
plot2

ggplot(
  plot_data,
  aes(x = median_diversion, fill = crop)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    position = "identity",
    alpha = 0.35,
    bins = 40,
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_d(option = "turbo", end = 0.9) +
  labs(
    x = "Field-level depletion depth (ft)",
    y = "Density",
    fill = "Crop",
    title = "Dispersion of Field-Level Depletion Depths by Crop",
    subtitle = "Great Salt Lake Basin, 2018–2024"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
