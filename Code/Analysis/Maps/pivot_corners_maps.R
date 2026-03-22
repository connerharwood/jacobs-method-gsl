
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

# Load pivot corners
pivot_corners = set_names(st_layers("Data/Clean/pivot_corners.gpkg")$name) |>
  map_dfr(
    \(x) st_read("Data/Clean/pivot_corners.gpkg", layer = x),
    .id = "county"
  )

# Load field-level annual depletions
load("Data/Clean/depletion_annual.rda")

# Filter to active fields in GSL Basin
depletion_annual = depletion_annual |> 
  filter(
    year > 2017, 
    id %in% pivot_corners$id, 
    crop != "Fallow/Idle",
    land_use_group %in% c("Active IR", "SubIRR", NA)
  )

# ==== PREP ====================================================================

# Calculate average annual depletion depth and volume for each field
corner_depletion = depletion_annual |> 
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
    pivot_corners |> select(id),
    by = "id",
    relationship = "one-to-one"
  ) |> 
  # Convert df to sf
  st_as_sf() |> 
  filter(!is.na(depletion_ft))

# Fetch basemap tile for GSL Basin
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldGrayCanvas",
  zoom = 10,
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

# Filter county spatial data to Box Elder County
boxelder = counties |> filter(county == "Box Elder")

# Fetch satellite imagery tile for Box Elder County basemap
boxelder_tile = get_tiles(
  boxelder,
  provider = "Esri.WorldGrayCanvas",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)

# Get spatial extent of tile
boxelder_tile_extent = ext(boxelder_tile)

# Convert tile to polygon then sf object
boxelder_tile_poly = as.polygons(boxelder_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(boxelder_tile))

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
  provider = "Esri.WorldGrayCanvas",
  zoom = 12,
  crop = TRUE,
  project = TRUE
)

# Get spatial extent of tile
cache_tile_extent = ext(cache_tile)

# Convert tile to polygon then sf object
cache_tile_poly = as.polygons(cache_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(cache_tile))

# Transform Cache County to align with tile's CRS, convert to terra vector
cache_vect = st_transform(cache, crs(cache_tile)) |> vect()

# Crop and mask satellite imagery tile to Cache County
cache_tile_crop = crop(cache_tile, cache_vect)
cache_tile_mask = mask(cache_tile_crop, cache_vect)

# ==== GSL BASIN CORNERS =======================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
corners_map = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = pivot_corners,
    fill = NA,
    color = "red"
  ) +
  geom_sf(
    data = basin_boundary,
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, GSL Basin") +
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
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
  )
corners_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/pivot_corners_map.png",
  plot = corners_map, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# Choropleth of field-level median annual depletion depth in GSL Basin
corners_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
  geom_sf(
    data = corner_depletion,
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
      min(corner_depletion$depletion_ft, na.rm = TRUE),
      max(corner_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression("0", "3.02"),
    limits = c(
      min(corner_depletion$depletion_ft, na.rm = TRUE),
      max(corner_depletion$depletion_ft, na.rm = TRUE)
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
corners_depth_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
corners_volume_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
  geom_sf(
    data = corner_depletion,
    aes(fill = depletion_af),
    color = NA
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(corner_depletion$depletion_af, na.rm = TRUE),
      max(corner_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression("0", "95.34"),
    limits = c(
      min(corner_depletion$depletion_af, na.rm = TRUE),
      max(corner_depletion$depletion_af, na.rm = TRUE)
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
corners_volume_choro

# ==== BOX ELDER CORNERS =======================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
boxelder_corners_map = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = boxelder_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = pivot_corners |> 
      filter(county == "Box Elder"),
      #st_filter(counties |> filter(county == "Box Elder"), .predicate = st_intersects),
    fill = NA,
    color = "red"
  ) +
  geom_sf(
    data = counties |> filter(county == "Box Elder"),
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, GSL Basin") +
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
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
  )
boxelder_corners_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/boxelder_corners_map.png",
  plot = boxelder_corners_map, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# Choropleth of field-level median annual depletion depth in GSL Basin
boxelder_corners_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = boxelder_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
  geom_sf(
    data = corner_depletion |> st_filter(counties |> filter(county == "Box Elder"), .predicate = st_intersects),
    aes(fill = depletion_ft),
    color = NA
  ) +
  geom_sf(
    data = counties |> filter(county == "Box Elder"),
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((corner_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
    ),
    labels = expression("0", "2.81"),
    limits = c(
      min((corner_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
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
boxelder_corners_depth_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
boxelder_corners_volume_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = boxelder_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
  geom_sf(
    data = corner_depletion |> st_filter(counties |> filter(county == "Box Elder"), .predicate = st_intersects),
    aes(fill = depletion_af),
    color = NA
  ) +
  geom_sf(
    data = counties |> filter(county == "Box Elder"),
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((corner_depletion |> filter(county == "Box Elder"))$depletion_af, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Box Elder"))$depletion_af, na.rm = TRUE)
    ),
    labels = expression("0", "63.69"),
    limits = c(
      min((corner_depletion |> filter(county == "Box Elder"))$depletion_af, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Box Elder"))$depletion_af, na.rm = TRUE)
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
boxelder_corners_volume_choro

# ==== CACHE CORNERS ===========================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
cache_corners_map = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = cache_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = pivot_corners |> 
      st_filter(counties |> filter(county == "Cache"), .predicate = st_intersects),
    fill = NA,
    color = "red"
  ) +
  geom_sf(
    data = counties |> filter(county == "Cache"),
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, GSL Basin") +
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
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
  )
cache_corners_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/cache_corners_map.png",
  plot = cache_corners_map, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# Choropleth of field-level median annual depletion depth in GSL Basin
cache_corners_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = cache_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
  geom_sf(
    data = corner_depletion |> filter(county == "Cache"),
    aes(fill = depletion_ft),
    color = NA
  ) +
  geom_sf(
    data = counties |> filter(county == "Cache"),
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((corner_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE)
    ),
    labels = expression("0.48", "2.20"),
    limits = c(
      min((corner_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Cache"))$depletion_ft, na.rm = TRUE)
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
cache_corners_depth_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
cache_corners_volume_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = cache_tile_mask) +
  # Color code each GSL Basin field by median annual depletion depth
  geom_sf(
    data = corner_depletion |> filter(county == "Cache"),
    aes(fill = depletion_af),
    color = NA
  ) +
  geom_sf(
    data = counties |> filter(county == "Cache"),
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((corner_depletion |> filter(county == "Cache"))$depletion_af, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Cache"))$depletion_af, na.rm = TRUE)
    ),
    labels = expression("0.27", "65.01"),
    limits = c(
      min((corner_depletion |> filter(county == "Cache"))$depletion_af, na.rm = TRUE),
      max((corner_depletion |> filter(county == "Cache"))$depletion_af, na.rm = TRUE)
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
cache_corners_volume_choro
