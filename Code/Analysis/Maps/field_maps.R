
library(tidyverse)
library(ggspatial)
library(shadowtext)
library(terra)
library(sf)
library(maptiles)
library(extrafont)
library(scales)

# ==== LOAD ====================================================================

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

# Load WRLU ag fields
fields = st_read("Data/Clean/fields_panel.gpkg") |> 
  # Filter to just 2024 cross section
  filter(year == 2024) |> 
  # Filter to fields that intersect GSL Basin
  st_filter(basin_boundary, .predicate = st_intersects)

# Load field-level annual depletions
load("Data/Clean/depletion_annual.rda")

# Filter to active fields in GSL Basin
depletion_annual = depletion_annual |> 
  filter(
    year > 2017, 
    id %in% fields$id, 
    crop != "Fallow/Idle",
    land_use_group %in% c("Active IR", "SubIRR", NA)
  )

# ==== PREP ====================================================================

# Calculate average annual depletion depth and volume for each field
field_depletion = depletion_annual |> 
  # Group data by field and county (county included just to retain it)
  group_by(id, county) |> 
  # Calculate median annual depletion depth and volume for each field
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Join with county spatial data
  right_join(
    fields |> select(id),
    by = "id",
    relationship = "one-to-one"
  ) |> 
  # Convert df to sf
  st_as_sf() |> 
  filter(!is.na(depletion_ft))

# Fetch satellite imagery tile for GSL Basin basemap
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)

# Transform Utah portion of GSL Basin to align with tile's CRS, convert to terra vector
basin_vect = st_transform(gsl_basin |> st_intersection(counties), crs(basin_tile)) |> vect()

# Crop and mask satellite imagery tile to GSL Basin
basin_tile_crop = crop(basin_tile, basin_vect)
basin_tile_mask = mask(basin_tile_crop, basin_vect)

# Filter county spatial data to Box Elder County
boxelder = counties |> filter(county == "Box Elder")

# Fetch satellite imagery tile for Box Elder County basemap
boxelder_tile = get_tiles(
  boxelder,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)
plot(boxelder_tile)

# Transform Box Elder County to align with tile's CRS, convert to terra vector
boxelder_vect = st_transform(boxelder, crs(boxelder_tile)) |> vect()

# Crop and mask satellite imagery tile to Box Elder County
boxelder_tile_crop = crop(boxelder_tile, boxelder_vect)
boxelder_tile_mask = mask(boxelder_tile_crop, boxelder_vect)

# Filter county spatial data to Cache County
cache = counties |> filter(county == "Cache")

# Fetch satellite imagery tile for Cache County basemap
cache_tile = get_tiles(
  cache,
  provider = "Esri.WorldImagery",
  zoom = 14,
  crop = TRUE,
  project = TRUE
)

# Transform Cache County to align with tile's CRS, convert to terra vector
cache_vect = st_transform(cache, crs(cache_tile)) |> vect()

# Crop and mask satellite imagery tile to Cache County
cache_tile_crop = crop(cache_tile, cache_vect)
cache_tile_mask = mask(cache_tile_crop, cache_vect)

field_ft_min = min(field_depletion$depletion_ft, na.rm = TRUE)
field_ft_max = max(field_depletion$depletion_ft, na.rm = TRUE)

boxelder_ft_min = min((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
boxelder_ft_max = max((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)

cache_ft_min = min((field_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE)
cache_ft_max = max((field_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE)

# ==== FIELDS CHORO ============================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
field_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = field_depletion,
    aes(fill = depletion_ft),
    color = NA
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(field_ft_min, field_ft_max),
    labels = c(comma(field_ft_min, accuracy = 1), comma(field_ft_max, accuracy = 0.01)),
    limits = c(field_ft_min, field_ft_max)
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
  # Minimalist ggplot theme
  theme_minimal() +
  # Customize plot elements
  theme(
    text = element_text(color = "black", family = "lato"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = -4),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.88, 0.9),
    legend.title = element_text(size = 14, vjust = 2),
    legend.text = element_text(size = 12),
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
field_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/field_depth_choro.png",
  plot = field_depth_choro,
  width = 16,
  height = 10,
  units = "in",
  dpi = 400
)
knitr::plot_crop("Figures/Maps/field_depth_choro.png")

# ==== BOX ELDER FIELDS CHORO ==================================================

# Choropleth of field-level median annual depletion depth in Box Elder County
boxelder_field_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = boxelder_tile_mask) +
  # Color code each Box Elder County field by median annual depletion depth
  geom_sf(
    data = field_depletion |> filter(county == "Box Elder"), 
    aes(fill = depletion_ft), 
    color = NA
  ) + 
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(boxelder_ft_min, boxelder_ft_max),
    labels = c(comma(boxelder_ft_min, accuracy = 1), comma(boxelder_ft_max, accuracy = 0.01)),
    limits = c(boxelder_ft_min, boxelder_ft_max)
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, Box Elder County", fill = "Depletion (AFA)") +
  # Minimalist ggplot theme
  theme_minimal() +
  # Customize plot elements
  theme(
    text = element_text(color = "black", family = "lato"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = -4),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5),
    legend.justification = c(0.3, 0.5),
    legend.title = element_text(size = 14, vjust = 2),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 5.5, r = 75, b = 5.5, l = 5.5)
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
boxelder_field_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/boxelder_field_depth_choro.png",
  plot = boxelder_field_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)
knitr::plot_crop("Figures/Maps/boxelder_field_depth_choro.png")

# ==== CACHE FIELDS CHORO ======================================================

# Choropleth of field-level median annual depletion depth in Cache County
cache_field_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = cache_tile_mask) +
  # Color code each Cache County field by median annual depletion depth
  geom_sf(
    data = field_depletion |> filter(county == "Cache"),
    aes(fill = depletion_ft),
    color = NA
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(cache_ft_min, cache_ft_max),
    labels = c(comma(cache_ft_min, accuracy = 1), comma(cache_ft_max, accuracy = 0.01)),
    limits = c(cache_ft_min, cache_ft_max)
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, Cache County", fill = "Depletion (AFA)") +
  # Minimalist ggplot theme
  theme_minimal() +
  # Customize plot elements
  theme(
    text = element_text(color = "black", family = "lato"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = -4),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(1, 0.5),
    legend.justification = c(0.1, 0.5),
    legend.title = element_text(size = 14, vjust = 2),
    legend.text = element_text(size = 12)
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
cache_field_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/cache_field_depth_choro.png",
  plot = cache_field_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)
knitr::plot_crop("Figures/Maps/cache_field_depth_choro.png")
