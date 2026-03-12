
library(tidyverse)
library(sf)
library(ggspatial)
library(shadowtext)
library(terra)
library(maptiles)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  filter(basin != "Strawberry") |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load Utah counties
counties = st_read("Data/Raw/Counties/Counties.shp") |> 
  # Select only county name
  select(county = NAME) |> 
  # Convert county name to title case
  mutate(county = str_to_title(county)) |> 
  # Remove Duchesne County
  filter(county != "Duchesne") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations and plotting
  st_transform(crs = 26912) |> 
  # Filter to counties that intersect GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

# Load pivot corners
pivot_corners = map_dfr(
  st_layers("Data/Clean/pivot_corners.gpkg")$name,
  \(x) st_read("Data/Clean/pivot_corners.gpkg", layer = x),
  .id = "county"
) |> 
  select(id) |> 
  st_transform(crs = 26912) |> 
  st_make_valid()

# ==== BOX ELDER AND CACHE =====================================================

# Fetch satellite imagery tile for GSL Basin basemap
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

# Choropleth of field-level median annual depletion depth in GSL Basin
pivot_corners_map = ggplot() +
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
pivot_corners_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/pivot_corners.png",
  plot = pivot_corners_plot, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)
