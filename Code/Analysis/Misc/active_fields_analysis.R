
library(tidyverse)
library(sf)
library(extrafont)
library(maptiles)
library(terra)
library(ggspatial)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load 2017-2024 ag fields panel
fields_panel = st_read("Data/Clean/fields_panel.gpkg")

# Load 2017-2024 annual field-level depletion
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

# ==== PREP ====================================================================

depletion_annual = depletion_annual |> filter(crop != "Fallow/Idle")

# Get each "Other" field's first non-Other, non-NA classification after 2018
replacement_lug <- depletion_annual |>
  semi_join(other_fields, by = "id") |>
  filter(year >= 2019, !is.na(land_use_group)) |>
  arrange(id, year) |>
  group_by(id) |>
  slice_head(n = 1) |>
  select(id, replacement = land_use_group)

# Then join back and overwrite "Other"
depletion_annual <- depletion_annual |>
  left_join(replacement_lug, by = "id") |>
  mutate(land_use_group = if_else(
    land_use_group == "Other" & !is.na(replacement),
    replacement,
    land_use_group
  )) |>
  select(-replacement)

lug_count = depletion_annual |> 
  group_by(land_use_group, year) |> 
  count() |> 
  pivot_wider(names_from = year, values_from = n)

# Pull the fields that were ever classified as "Other"
other_fields <- depletion_annual |>
  filter(land_use_group == "Other") |>
  distinct(id)  # use whatever your unique field identifier is

# Now look at their full history
other_history <- depletion_annual |>
  semi_join(other_fields, by = "id") |>
  group_by(land_use_group, year) |>
  count() |>
  pivot_wider(names_from = year, values_from = n)

depletion_annual |>
  semi_join(other_fields, by = "id") |>
  filter(year %in% c(2017, 2018, 2019, 2020)) |>
  pivot_wider(names_from = year, values_from = land_use_group, 
              names_prefix = "lug_") |>
  count(lug_2017, lug_2018, lug_2019, lug_2020)





# Join depletion data with field polygons
depletion_annual_sf = depletion_annual |> 
  left_join(
    fields_panel |> select(id, year, geom),
    by = c("id", "year"),
    relationship = "one-to-one"
  ) |> 
  st_as_sf()

not_fallow_fields = depletion_annual_sf |> filter(crop != "Fallow/Idle" | is.na(crop))
fallow_fields = depletion_annual_sf |> filter(crop == "Fallow/Idle")

all_fields_lug_ave = depletion_annual_sf |> 
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

fallow_fields_lug_ave = fallow_fields |> 
  st_drop_geometry() |> 
  group_by(land_use_group) |> 
  summarize(
    depletion_ft_median = median(depletion_ft, na.rm = TRUE),
    depletion_ft_mean = mean(depletion_ft, na.rm = TRUE),
    .groups = "drop"
  )

fields_map_data = not_fallow_fields |> 
  # Filter to 2024 fields
  filter(year == 2024) |> 
  # Filter to fields that intersect GSL Basin
  st_filter(basin_boundary, .predicate = st_intersects) |> 
  # Classify each field as dry or irrigated
  mutate(
    class = case_when(
      land_use_group == "Dry Ag" ~ "Dry Ag",
      land_use_group %in% c("Active IR", "SubIRR", NA) ~ "Irrigated"
    )
  )

# Fetch satellite imagery tile for GSL Basin basemap
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)

# Get spatial extent of tile
basin_tile_extent = ext(basin_tile)

# Convert tile to polygon then sf object
basin_tile_poly = as.polygons(basin_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(basin_tile))

# Transform Utah portion of GSL Basin to align with tile's CRS, convert to terra vector
basin_vect = st_transform(gsl_basin |> st_intersection(counties), crs(basin_tile)) |> vect()

# Crop and mask satellite imagery tile to GSL Basin
basin_tile_crop = crop(basin_tile, basin_vect)
basin_tile_mask = mask(basin_tile_crop, basin_vect)

# ==== GRAPHS ==================================================================

all_fields_plot = ggplot() + 
  geom_density(
    data = depletion_annual_sf,
    aes(x = depletion_ft, fill = land_use_group, color = land_use_group),
    linewidth = 1,
    alpha = 0.4
  ) +
  geom_vline(
    data = all_fields_lug_ave,
    aes(xintercept = depletion_ft_median, color = land_use_group),
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.78, 
    y = 0.6, 
    label = "\u2190",
    size = 14 / 2.845
  ) +
  annotate(
    "text",
    x = 1.92,
    y = 0.6,
    label = "Median",
    size = 14 / 2.845,
    family = "lato"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Depletion by WRLU Land Use Group, All Fields",
    x = "Depletion (AFA)",
    y = "Density",
    fill = "LU Group",
    color = "LU Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "black", family = "lato"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = c(0.85, 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )
all_fields_plot

not_fallow_fields_plot = ggplot() + 
  geom_density(
    data = not_fallow_fields,
    aes(x = depletion_ft, fill = land_use_group, color = land_use_group),
    linewidth = 1,
    alpha = 0.4
  ) +
  geom_vline(
    data = not_fallow_fields_lug_ave,
    aes(xintercept = depletion_ft_median, color = land_use_group),
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.78, 
    y = 0.6, 
    label = "\u2190",
    size = 14 / 2.845
  ) +
  annotate(
    "text",
    x = 1.92,
    y = 0.6,
    label = "Median",
    size = 14 / 2.845,
    family = "lato"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Depletion by WRLU Land Use Group, crop != \"Fallow/Idle\" | is.na(crop)",
    x = "Depletion (AFA)",
    y = "Density",
    fill = "LU Group",
    color = "LU Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "black", family = "lato"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = c(0.85, 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )
not_fallow_fields_plot

fallow_fields_plot = ggplot() + 
  geom_density(
    data = fallow_fields,
    aes(x = depletion_ft, fill = land_use_group, color = land_use_group),
    linewidth = 1,
    alpha = 0.4
  ) +
  geom_vline(
    data = fallow_fields_lug_ave,
    aes(xintercept = depletion_ft_median, color = land_use_group),
    linewidth = 0.7,
    linetype = "dashed",
    show.legend = FALSE
  ) + 
  annotate(
    "text", 
    x = 0.68, 
    y = 1.3, 
    label = "\u2190",
    size = 14 / 2.845
  ) +
  annotate(
    "text",
    x = 0.81,
    y = 1.3,
    label = "Median",
    size = 14 / 2.845,
    family = "lato"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Depletion by WRLU Land Use Group, crop == \"Fallow/Idle\"",
    x = "Depletion (AFA)",
    y = "Density",
    fill = "LU Group",
    color = "LU Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "black", family = "lato"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(color = "black", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = c(0.85, 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )
fallow_fields_plot

# Save plot
ggsave(
  "Figures/Graphs/all_fields_lug_density.png", 
  plot = all_fields_plot,
  dpi = 500,
  width = 16.5,
  height = 10.3,
  bg = "white"
)

# Save plot
ggsave(
  "Figures/Graphs/not_fallow_fields_lug_density.png", 
  plot = not_fallow_fields_plot,
  dpi = 500,
  width = 16.5,
  height = 10.3,
  bg = "white"
)

# Save plot
ggsave(
  "Figures/Graphs/fallow_fields_lug_density.png", 
  plot = fallow_fields_plot,
  dpi = 500,
  width = 16.5,
  height = 10.3,
  bg = "white"
)

# ==== MAPS ====================================================================

# Map of dry and irrigated fields
field_lug_map = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Fields colored by class
  geom_sf(
    data = fields_map_data,
    aes(fill = class),
    color = NA
  ) +
  # Two-category color scale
  scale_fill_manual(
    values = c(
      "Dry Ag" = "red",
      "Irrigated" = "blue"
    ),
    na.value = "darkgray"
  ) +
  # Add plot and legend titles
  labs(title = "WRLU Land Use Group, Irrigated vs. Dry Ag", fill = "LU Group") +
  # Customize plot elements
  theme(
    text = element_text(color = "black", family = "lato"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = -4),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.94, 0.75),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )
field_lug_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/field_lug_map.png",
  plot = field_lug_map, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# Interactive map of dry vs. irrigated fields
tm_shape(fields_map_data |> filter(class == "Dry Ag")) +
  tm_basemap("Esri.WorldImagery") +
  tm_borders(col = "red", lwd = 1) +
  tm_shape(fields_map_data |> filter(class == "Irrigated")) +
  tm_borders(col = "blue", lwd = 1)
