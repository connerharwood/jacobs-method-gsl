
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
  # Select only basin name
  select(basin = Name) |> 
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
  st_transform(crs = 26912)

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

# Load each county's pivot corners
box_elder_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Box Elder")
cache_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Cache")
davis_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Davis")
duchesne_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Duchesne")
juab_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Juab")
morgan_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Morgan")
rich_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Rich")
salt_lake_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Salt Lake")
sanpete_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Sanpete")
summit_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Summit")
tooele_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Tooele")
utah_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Utah")
wasatch_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Wasatch")
weber_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Weber")

# Combine Cache and Box Elder corners into one sf object
pivot_corners = rbind(
  box_elder_corners,
  cache_corners,
  davis_corners,
  duchesne_corners,
  juab_corners,
  morgan_corners,
  rich_corners,
  salt_lake_corners,
  sanpete_corners,
  summit_corners,
  tooele_corners,
  utah_corners,
  wasatch_corners,
  weber_corners
) |> 
  select(id)

# ==== BOX ELDER AND CACHE =====================================================

# Fetch satellite imagery tile for GSL Basin basemap
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldGrayCanvas",
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
basin_vect = st_transform(basin_boundary, crs(basin_tile)) |> vect()

# Crop and mask satellite imagery tile to GSL Basin
basin_tile_crop = crop(basin_tile, basin_vect)
basin_tile_mask = mask(basin_tile_crop, basin_vect)

# Choropleth of field-level median annual depletion depth in GSL Basin
pivot_corners_plot = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = pivot_corners, , 
    color = "red",
    fill = "red"
  ) +
  geom_sf(data = basin_boundary, color = "black", fill = NA) +
  # # Add county boundaries
  # geom_sf(
  #   data = counties, 
  #   color = "black",
  #   fill = NA, 
  #   linewidth = 0.2
  # ) +
  # # Add GSL
  # geom_sf(
  #   data = gsl_basin |> filter(basin == "Great Salt Lake"), 
  #   color = "darkblue",
  #   fill = "white", 
  #   linewidth = 0.4
  # ) +
  # Add GSL Basin boundary
  # geom_sf(
  #   data = basin_boundary,
  #   color = "grey",
  #   fill = NA,
  #   linewidth = 0.2
  # ) +
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
    #legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    #legend.text = element_text(size = 12), # Adjust legend tick labels
    plot.title = element_blank() # Adjust plot title
  )
pivot_corners_plot

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/pivot_corners.png",
  plot = pivot_corners_plot, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# ==== CACHE ===================================================================

# Filter county spatial data to Cache County
cache = counties |> filter(county == "Cache")

# Fetch satellite imagery tile for Cache County basemap
cache_tile = get_tiles(
  cache,
  provider = "Esri.WorldTopoMap",
  zoom = 11,
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

# Choropleth of field-level median annual depletion depth in GSL Basin
cache_corners_plot = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = cache_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = pivot_corners |> filter(county == "Cache"), 
    color = "red",
    fill = NA
  ) +
  # # Add county boundaries
  # geom_sf(
  #   data = counties, 
  #   color = "black",
  #   fill = NA, 
  #   linewidth = 0.2
  # ) +
  # Add plot and legend titles
  labs(title = "Cache County Pivot Corners") +
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
    #legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    #legend.text = element_text(size = 12), # Adjust legend tick labels
    plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
  )
cache_corners_plot
