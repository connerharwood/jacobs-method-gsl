
library(tidyverse)
library(sf)
library(terra)
library(glue)

# ==== LOAD ====================================================================

# Load temporary fields panel
fields_panel_temp = st_read("Data/Clean/Fields/Utah/Temp/fields_panel_temp.gpkg")

# ==== REMOVE NON-IRRIGATED AREAS FROM FIELDS ==================================

# Initialize list to store each year's masked fields
fields_panel_list = list()

for (yr in 2017:2024) {
  # Load IrrMapper raster for specified year
  irrmapper = rast(glue("Data/IrrMapper/Utah/Raw TIFF Files/utah_irrmapper_{yr}.tif"))
  
  # Load fields for specified year
  fields = fields_panel_temp |> 
    filter(year == yr) |> 
    st_transform(crs = crs(irrmapper))
  
  # Convert fields to raster
  fields_rast = rasterize(
    vect(fields),
    irrmapper,
    field = "id"
  )
  
  # Mask non-irrigated pixels
  irr_masked = irrmapper
  irr_masked[irr_masked[] != 1] = NA
  fields_irr_rast = mask(fields_rast, irr_masked)
  
  # Convert fields back to sf
  fields_irr_sf = as.polygons(fields_irr_rast, na.rm = TRUE) |> 
    st_as_sf() |> 
    left_join(
      fields |> st_drop_geometry(),
      by = "id",
      relationship = "one-to-one"
    ) |> 
    mutate(year = yr)
  
  fields_panel_list[[as.character(yr)]] = fields_irr_sf
}

fields_irrmapper_panel = bind_rows(fields_panel_list)

# ==== SAVE ====================================================================

st_write(fields_irrmapper_panel, "Data/IrrMapper/Utah/fields_irrmapper_panel.gpkg", delete_layer = TRUE)



filtered_fields = st_read("Data/IrrMapper/Utah/fields_irrmapper_panel.gpkg")
all_fields = fields_panel_temp

missing_fields = anti_join(
  all_fields,
  filtered_fields |> st_drop_geometry(),
  by = c("id", "year")
)

check = missing_fields |> 
  st_drop_geometry() |> 
  group_by(crop) |> 
  count()
