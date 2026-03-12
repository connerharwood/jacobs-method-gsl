
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

fields_panel = st_read("Data/Clean/fields_panel.gpkg")
load("Data/Clean/depletion_annual.rda")

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  # Select only basin name
  select(basin = Name) |> 
  # Remove Strawberry Basin
  filter(basin != "Strawberry") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations and plotting
  st_transform(crs = 26912)

# Load Utah counties
counties = st_read("Data/Raw/Counties/Counties.shp") |> 
  # Select only county name
  select(county = NAME) |> 
  # Convert county name to title case
  mutate(county = str_to_title(county)) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations and plotting
  st_transform(crs = 26912) |> 
  # Filter to counties that intersect GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects) |> 
  # Remove Duchesne County
  filter(county != "Duchesne")

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

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






# Load WRLU ag fields
fields_panel = st_read("Data/Clean/fields_panel.gpkg") |> 
  # Filter to fields that intersect GSL Basin
  st_filter(basin_boundary, .predicate = st_intersects) |> 
  # Remove fallow/idle fields
  filter(crop != "Fallow/Idle") |>
  # Classify each field as dry or irrigated
  mutate(
    class = case_when(
      land_use_group == "Dry Ag" ~ "Dry Ag",
      land_use_group %in% c("Active IR", "SubIRR", NA) ~ "Irrigated"
    )
  )

# Map of dry and irrigated fields
field_lu_map = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Fields colored by class
  geom_sf(
    data = fields_panel |> filter(year == 2024),
    aes(fill = class),
    color = NA
  ) +
  # Two-category color scale
  scale_fill_manual(
    values = c(
      "Dry Ag" = "red",     # tan / dryland
      "Irrigated" = "blue"   # blue-green irrigated
    ),
    na.value = "darkgray"
  ) +
  # Create separate plot for each year
  #facet_wrap(~year) +
  # Add plot and legend titles
  labs(title = "Field Classification, Cache County", fill = "LU Group") +
  # Customize plot elements
  theme(
    panel.grid.major = element_blank(), # Remove major panel grids
    panel.grid.minor = element_blank(), # Remove minor panel grids
    axis.text = element_blank(), # Remove axes text
    axis.ticks = element_blank(), # Remove axes ticks
    axis.title = element_blank(), # Remove axes titles
    panel.background = element_rect(fill = "white", color = NA), # Create white background
    plot.background = element_rect(fill = "white", color = NA), # Create white background
    legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    legend.text = element_text(size = 12), # Adjust legend tick labels
    plot.title = element_blank(), # Adjust plot title
    text = element_text(color = "black", family = "Lato"),
  )
field_lu_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/field_lu_map.png",
  plot = field_lu_map, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

tm_shape(fields_panel |> filter(year == 2024, class == "Dry Ag")) +
  tm_basemap("Esri.WorldImagery") +
  tm_borders(col = "red", lwd = 1) +
  tm_shape(fields_panel |> filter(year == 2024, class == "Irrigated")) +
  tm_borders(col = "blue", lwd = 1)
