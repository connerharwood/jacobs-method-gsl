
library(tidyverse)
library(sf)
library(mapedit)
library(leaflet)

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

# ==== BOX ELDER ===============================================================

# Load originally identified Box Elder pivot corners
boxelder_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Box Elder") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
boxelder_misidentified = selectFeatures(
  x = boxelder_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
boxelder_missed = selectFeatures(
  x = fields |> filter(county == "Box Elder", !id %in% boxelder_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined Box Elder corners
boxelder_corners = boxelder_original |> 
  filter(!id %in% boxelder_misidentified$id) |> 
  rbind(boxelder_missed)

# Save Box Elder County pivot corners
st_write(
  boxelder_corners,
  "Data/Clean/pivot_corners.gpkg",
  layer = "Box Elder",
  delete_layer = TRUE
)

# ==== CACHE ===================================================================

# Load originally identified Cache pivot corners
cache_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Cache") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
cache_misidentified = selectFeatures(
  x = cache_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
cache_missed = selectFeatures(
  x = fields |> filter(county == "Cache", !id %in% cache_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined Cache corners
cache_corners = cache_original |> 
  filter(!id %in% cache_misidentified$id) |> 
  rbind(cache_missed)

# Save Cache County pivot corners
st_write(
  cache_corners,
  "Data/Clean/pivot_corners.gpkg",
  layer = "Cache",
  delete_layer = TRUE
)

# ==== CARBON ==================================================================

# Load originally identified Carbon pivot corners
carbon_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Carbon") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
carbon_misidentified = selectFeatures(
  x = carbon_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
carbon_missed = selectFeatures(
  x = fields |> filter(county == "Carbon", !id %in% carbon_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined Carbon corners
carbon_corners = carbon_original |> 
  filter(!id %in% carbon_misidentified$id) |> 
  rbind(carbon_missed)

# Save Carbon County pivot corners
st_write(
  carbon_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Carbon", 
  delete_layer = TRUE
)

# ==== DAVIS ===================================================================

# Load originally identified Davis pivot corners
davis_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Davis") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
davis_misidentified = selectFeatures(
  x = davis_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
davis_missed = selectFeatures(
  x = fields |> filter(county == "Davis", !id %in% davis_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined Davis corners
davis_corners = davis_original |> 
  st_transform(crs = 26912) |> 
  filter(!id %in% davis_misidentified$id) |> 
  rbind(davis_missed)

# Save Davis County pivot corners
st_write(
  davis_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Davis", 
  delete_layer = TRUE
)

# ==== JUAB ====================================================================

# Load originally identified pivot corners
juab_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Juab") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
juab_misidentified = selectFeatures(
  x = juab_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
juab_missed = selectFeatures(
  x = fields |> filter(county == "Juab", !id %in% juab_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
juab_corners = juab_original |> 
  filter(!id %in% juab_misidentified$id) |> 
  rbind(juab_missed)

# Save Juab County pivot corners
st_write(
  juab_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Juab", 
  delete_layer = TRUE
)

# ==== MORGAN ==================================================================

# Load originally identified pivot corners
morgan_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Morgan") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
morgan_misidentified = selectFeatures(
  x = morgan_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
morgan_missed = selectFeatures(
  x = fields |> filter(county == "Morgan", !id %in% morgan_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
morgan_corners = morgan_original |> 
  filter(!id %in% morgan_misidentified$id) |> 
  rbind(morgan_missed)

# Save Morgan County pivot corners
st_write(
  morgan_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Morgan", 
  delete_layer = TRUE
)

# ==== RICH ====================================================================

# Load originally identified pivot corners
rich_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Rich") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
rich_misidentified = selectFeatures(
  x = rich_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
rich_missed = selectFeatures(
  x = fields |> filter(county == "Rich", !id %in% rich_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
rich_corners = rich_original |> 
  filter(!id %in% rich_misidentified$id) |> 
  rbind(rich_missed)

# Save Rich County pivot corners
st_write(
  rich_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Rich", 
  delete_layer = TRUE
)

# ==== SALT LAKE ===============================================================

# Load originally identified pivot corners
saltlake_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Salt Lake") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
saltlake_misidentified = selectFeatures(
  x = saltlake_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
saltlake_missed = selectFeatures(
  x = fields |> filter(county == "Salt Lake", !id %in% saltlake_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
saltlake_corners = saltlake_original |> 
  filter(!id %in% saltlake_misidentified$id) |> 
  rbind(saltlake_missed)

# Save Salt Lake County pivot corners
st_write(
  saltlake_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Salt Lake", 
  delete_layer = TRUE
)

# ==== SANPETE =================================================================

# Load originally identified pivot corners
sanpete_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Sanpete") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
sanpete_misidentified = selectFeatures(
  x = sanpete_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
sanpete_missed = selectFeatures(
  x = fields |> filter(county == "Sanpete", !id %in% sanpete_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
sanpete_corners = sanpete_original |> 
  #filter(!id %in% sanpete_misidentified$id) |> 
  rbind(sanpete_missed)

# Save Sanpete County pivot corners
st_write(
  sanpete_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Sanpete", 
  delete_layer = TRUE
)

# ==== SUMMIT ==================================================================

# Load originally identified pivot corners
summit_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Summit") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
summit_misidentified = selectFeatures(
  x = summit_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
summit_missed = selectFeatures(
  x = fields |> filter(county == "Summit", !id %in% summit_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
summit_corners = summit_original |> 
  filter(!id %in% summit_misidentified$id) |> 
  rbind(summit_missed)

# Save Summit County pivot corners
st_write(
  summit_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Summit", 
  delete_layer = TRUE
)

# ==== TOOELE ==================================================================

# Load originally identified pivot corners
tooele_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Tooele") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
tooele_misidentified = selectFeatures(
  x = tooele_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
tooele_missed = selectFeatures(
  x = fields |> filter(county == "Tooele", !id %in% tooele_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
tooele_corners = tooele_original |> 
  filter(!id %in% tooele_misidentified$id) |> 
  rbind(tooele_missed)

# Save Tooele County pivot corners
st_write(
  tooele_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Tooele", 
  delete_layer = TRUE
)

# ==== UTAH ====================================================================

# Load originally identified pivot corners
utah_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Utah") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
utah_misidentified = selectFeatures(
  x = utah_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
utah_missed = selectFeatures(
  x = fields |> filter(county == "Utah", !id %in% utah_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
utah_corners = utah_original |> 
  filter(!id %in% utah_misidentified$id) |> 
  rbind(utah_missed)

# Save Utah County pivot corners
st_write(
  utah_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Utah", 
  delete_layer = TRUE
)

# ==== WASATCH =================================================================

# Load originally identified pivot corners
wasatch_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Wasatch") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
wasatch_misidentified = selectFeatures(
  x = wasatch_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
wasatch_missed = selectFeatures(
  x = fields |> filter(county == "Wasatch", !id %in% wasatch_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
wasatch_corners = wasatch_original |> 
  filter(!id %in% wasatch_misidentified$id) |> 
  rbind(wasatch_missed)

# Save Wasatch County pivot corners
st_write(
  wasatch_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Wasatch", 
  delete_layer = TRUE
)

# ==== WEBER ===================================================================

# Load originally identified pivot corners
weber_original = st_read("Data/Clean/pivot_corners.gpkg", layer = "Weber") |> st_transform(crs = 26912)

# Fields incorrectly identified as corners
weber_misidentified = selectFeatures(
  x = weber_original,
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Fields missed as corners
weber_missed = selectFeatures(
  x = fields |> filter(county == "Weber", !id %in% weber_original$id),
  mode = "click",
  map = leaflet() |> addProviderTiles("Esri.WorldImagery")
) |> st_transform(crs = 26912)

# Refined corners
weber_corners = weber_original |> 
  filter(!id %in% weber_misidentified$id) |> 
  rbind(weber_missed)

# Save Weber County pivot corners
st_write(
  weber_corners, 
  "Data/Clean/pivot_corners.gpkg", 
  layer = "Weber", 
  delete_layer = TRUE
)
