
library(tidyverse)
library(ggspatial)
library(shadowtext)
library(terra)
library(sf)
library(maptiles)
library(extrafont)

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

# ==== FIELDS CHORO ============================================================

# Fetch satellite imagery tile for GSL Basin basemap
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)

# Esri.WorldImagery zoom = 11
# Esri.WorldTopoMap zoom = 10
# Esri.WorldGrayCanvas zoom = 11

# Get spatial extent of tile
basin_tile_extent = ext(basin_tile)

# Convert tile to polygon then sf object
basin_tile_poly = as.polygons(basin_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(basin_tile))

# Transform Utah portion of GSL Basin to align with tile's CRS, convert to terra vector
basin_vect = st_transform(gsl_basin |> st_intersection(counties), crs(basin_tile)) |> vect()

# Crop and mask satellite imagery tile to GSL Basin
basin_tile_crop = crop(basin_tile, basin_vect)
basin_tile_mask = mask(basin_tile_crop, basin_vect)

# Choropleth of field-level median annual depletion depth in GSL Basin
field_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
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
    breaks = c(
      min(field_depletion$depletion_ft, na.rm = TRUE),
      max(field_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression("0", "3.64"),
    limits = c(
      min(field_depletion$depletion_ft, na.rm = TRUE),
      max(field_depletion$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
  # Minimalist ggplot theme
  theme_minimal() +
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
  "Figures/Maps/field_depth_choro_WorldImagery.png",
  plot = field_depth_choro,
  width = 16,
  height = 10,
  units = "in",
  dpi = 400
)

# ==== BOX ELDER FIELDS CHORO ==================================================

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

# Esri.WorldImagery zoom = 11
# Esri.WorldTopoMap zoom = 10
# Esri.WorldGrayCanvas zoom = 10

# Get spatial extent of tile
boxelder_tile_extent = ext(boxelder_tile)

# Convert tile to polygon then sf object
boxelder_tile_poly = as.polygons(boxelder_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(boxelder_tile))

# Transform Box Elder County to align with tile's CRS, convert to terra vector
boxelder_vect = st_transform(boxelder, crs(boxelder_tile)) |> vect()

# Crop and mask satellite imagery tile to Box Elder County
boxelder_tile_crop = crop(boxelder_tile, boxelder_vect)
boxelder_tile_mask = mask(boxelder_tile_crop, boxelder_vect)

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
  # # Add Box Elder County boundary
  # geom_sf(
  #   data = boxelder,
  #   fill = NA,
  #   color = "black",
  #   linewidth = 0.3
  # ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE),
      #0.8, 1.6, 2.4,
      max((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "3.38"
    ),
    limits = c(
      min((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE),
      max((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, Box Elder County", fill = "Depletion (AFA)") +
  # Minimalist ggplot theme
  theme_minimal() +
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
    # plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
    plot.title = element_blank(),
    text = element_text(color = "black", family = "Lato")
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
  "Figures/Maps/boxelder_field_depth_choro_WorldImagery.png",
  plot = boxelder_field_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# ==== CACHE FIELDS CHORO ======================================================

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

# Esri.WorldImagery zoom = 14
# Esri.WorldTopoMap zoom = 11
# Esri.WorldGrayCanvas zoom = 11

# Get spatial extent of tile
cache_tile_extent = ext(cache_tile)

# Convert tile to polygon then sf object
cache_tile_poly = as.polygons(cache_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(cache_tile))

# Transform Cache County to align with tile's CRS, convert to terra vector
cache_vect = st_transform(cache, crs(cache_tile)) |> vect()

# Crop and mask satellite imagery tile to Cache County
cache_tile_crop = crop(cache_tile, cache_vect)
cache_tile_mask = mask(cache_tile_crop, cache_vect)

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
  # # Add Cache County boundary
  # geom_sf(
  #   data = cache,
  #   fill = NA,
  #   color = "black",
  #   linewidth = 0.3
  # ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((field_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE),
      #0.8, 1.6, 2.4,
      max((field_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "3.29"
    ),
    limits = c(
      min((field_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE),
      max((field_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, Cache County", fill = "Depletion (AFA)") +
  # Minimalist ggplot theme
  theme_minimal() +
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
    #plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
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
  "Figures/Maps/cache_field_depth_choro_WorldImagery.png",
  plot = cache_field_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)
