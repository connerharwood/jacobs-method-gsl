
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

fields_panel = st_read("Data/Clean/fields_panel.gpkg")
load("Data/Clean/depletion_annual.rda")

# ==== VARIABLE ANALYSIS =======================================================

fields = fields_panel |> 
  left_join(
    depletion_annual |> select(id, year, depletion_ft, depletion_af),
    by = c("id", "year")
  )

all_fields = fields
not_fallow_fields = fields |> filter(crop != "Fallow/Idle")

fallow_fields = fields |> 
  st_drop_geometry() |> 
  anti_join(not_fallow_fields |> st_drop_geometry())

all_fields_lug_ave = all_fields |> 
  st_drop_geometry() |> 
  group_by(land_use_group) |> 
  summarize(
    depletion_ft_median = median(depletion_ft, na.rm = TRUE),
    depletion_ft_mean = mean(depletion_ft, na.rm = TRUE),
    .groups = "drop"
  )

not_fallow_fields_lug_ave = not_fallow_fields |> 
  st_drop_geometry() |> 
  group_by(land_use_group) |> 
  summarize(
    depletion_ft_median = median(depletion_ft, na.rm = TRUE),
    depletion_ft_mean = mean(depletion_ft, na.rm = TRUE),
    .groups = "drop"
  )

all_fields_plot = ggplot() + 
  geom_density(
    data = all_fields,
    aes(x = depletion_ft, fill = land_use_group, color = land_use_group),
    linewidth = 1,
    alpha = 0.4
  ) +
  geom_vline(
    data = all_fields_lug_ave,
    aes(xintercept = depletion_ft_mean, color = land_use_group),
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) + 
  theme_minimal() +
  labs(
    x = "Depletion (AFA)",
    y = "Density",
    fill = "WRLU Land Use Group",
    color = "WRLU Land Use Group"
  )
all_fields_plot

# Save plot
ggsave(
  "Figures/Plots/all_fields_plot.png", 
  plot = all_fields_plot,
  dpi = 500,
  width = 16.5,
  height = 10.3,
  bg = "white"
)

not_fallow_fields_plot = ggplot() + 
  geom_density(
    data = not_fallow_fields,
    aes(x = depletion_ft, fill = land_use_group, color = land_use_group),
    linewidth = 1,
    alpha = 0.4
  ) +
  geom_vline(
    data = not_fallow_fields_lug_ave,
    aes(xintercept = depletion_ft_mean, color = land_use_group),
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) + 
  theme_minimal() +
  labs(
    x = "Depletion (AFA)",
    y = "Density",
    fill = "WRLU Land Use Group",
    color = "WRLU Land Use Group"
  )
not_fallow_fields_plot

# Save plot
ggsave(
  "Figures/Plots/not_fallow_fields_plot.png", 
  plot = not_fallow_fields_plot,
  dpi = 500,
  width = 16.5,
  height = 10.3,
  bg = "white"
)

