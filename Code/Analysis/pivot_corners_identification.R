
library(tidyverse)
library(sf)
library(mapedit)
library(leaflet)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  filter(basin != "Strawberry") |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load 2024 WRLU fields
fields = st_read("Data/Clean/fields_panel.gpkg") |>
  # Filter to just 2024
  filter(year == 2024) |> 
  # Filter to fields within GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# ==== BOX ELDER COUNTY ========================================================

# Interactively select pivot corners in Box Elder County
box_elder_corners = selectFeatures(
  x = fields |> filter(county == "Box Elder"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Box Elder County pivot corners
st_write(
  box_elder_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Box Elder", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== CACHE COUNTY ============================================================

# Interactively select pivot corners in Cache County
cache_corners = selectFeatures(
  x = fields |> filter(county == "Cache"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Cache County pivot corners
st_write(
  cache_corners,
  "Data/Clean/pivot_corners.gpkg",
  layer = "Cache",
  delete_layer = TRUE,
  delete_dsn = FALSE
)

# DONE

# ==== CARBON COUNTY ===========================================================

# Interactively select pivot corners in Carbon County
carbon_corners = selectFeatures(
  x = fields |> filter(county == "Carbon"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Carbon County pivot corners
st_write(
  carbon_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Carbon", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== DAVIS COUNTY ============================================================

# Interactively select pivot corners in Davis County
davis_corners = selectFeatures(
  x = fields |> filter(county == "Davis"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Davis County pivot corners
st_write(
  davis_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Davis", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== DUCHESNE COUNTY =========================================================

# Interactively select pivot corners in Duchesne County
duchesne_corners = selectFeatures(
  x = fields |> filter(county == "Duchesne"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Duchesne County pivot corners
st_write(
  duchesne_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Duchesne", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== JUAB COUNTY =============================================================

# Interactively select pivot corners in Juab County
juab_corners = selectFeatures(
  x = fields |> filter(county == "Juab"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Juab County pivot corners
st_write(
  juab_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Juab", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== MORGAN COUNTY ===========================================================

# Interactively select pivot corners in Morgan County
morgan_corners = selectFeatures(
  x = fields |> filter(county == "Morgan"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Morgan County pivot corners
st_write(
  morgan_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Morgan", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== RICH COUNTY =============================================================

# Interactively select pivot corners in Rich County
rich_corners = selectFeatures(
  x = fields |> filter(county == "Rich"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Rich County pivot corners
st_write(
  rich_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Rich", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== SALT LAKE COUNTY ========================================================

# Interactively select pivot corners in Salt Lake County
salt_lake_corners = selectFeatures(
  x = fields |> filter(county == "Salt Lake"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Salt Lake County pivot corners
st_write(
  salt_lake_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Salt Lake", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== SANPETE COUNTY ==========================================================

# Interactively select pivot corners in Sanpete County
sanpete_corners = selectFeatures(
  x = fields |> filter(county == "Sanpete"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Sanpete County pivot corners
st_write(
  sanpete_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Sanpete", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== SUMMIT COUNTY ===========================================================

# Interactively select pivot corners in Summit County
summit_corners = selectFeatures(
  x = fields |> filter(county == "Summit"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Summit County pivot corners
st_write(
  summit_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Summit", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== TOOELE COUNTY ===========================================================

# Interactively select pivot corners in Tooele County
tooele_corners = selectFeatures(
  x = fields |> filter(county == "Tooele"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Tooele County pivot corners
st_write(
  tooele_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Tooele", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== UTAH COUNTY =============================================================

# Interactively select pivot corners in Utah County
utah_corners = selectFeatures(
  x = fields |> filter(county == "Utah"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Utah County pivot corners
st_write(
  utah_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Utah", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== WASATCH COUNTY ============================================================

# Interactively select pivot corners in Wasatch County
wasatch_corners = selectFeatures(
  x = fields |> filter(county == "Wasatch"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Wasatch County pivot corners
st_write(
  wasatch_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Wasatch", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE

# ==== WEBER COUNTY ============================================================

# Interactively select pivot corners in Weber County
weber_corners = selectFeatures(
  x = fields |> filter(county == "Weber"),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
)

# Save Weber County pivot corners
st_write(
  weber_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Weber", 
  delete_layer = TRUE, 
  delete_dsn = FALSE
)

# DONE
