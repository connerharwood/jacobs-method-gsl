
library(tidyverse)
library(sf)
library(terra)
library(glue)

# ==== LOAD ====================================================================

# Load temporary fields panel
fields_panel_temp = st_read("Data/Clean/Fields/Utah/Temp/fields_panel_temp.gpkg")

# ==== COMBINE IRRMAPPER =======================================================

# Initialize list to store each IrrMapper raster
irr_list = list()

# For each year, load IrrMapper and store in list
for (yr in 2017:2024) {
  irr = rast(glue("Data/IrrMapper/Utah/Raw TIFF Files/utah_irrmapper_{yr}.tif"))
  irr_list[[as.character(yr)]] = irr
}

# Stack all IrrMapper years into one raster
irr_stack = rast(irr_list)

# Assign pixels that were ever irrigated as 1
irr_union = app(irr_stack, fun = function(x) as.numeric(any(x == 1, na.rm = TRUE)))

# Set non-irrigated pixels to NA
irr_union[irr_union[] != 1] = NA

# ==== REMOVE NON-IRRIGATED AREAS FROM FIELDS ==================================

# Initialize list to store each year's masked fields
fields_panel_list = list()

for (yr in 2017:2024) {
  
  # Subset fields to current year and match raster CRS
  fields = fields_panel_temp |> 
    filter(year == yr) |> 
    st_transform(crs = crs(irr_union))
  
  # Rasterize fields
  fields_rast = rasterize(
    vect(fields),
    irr_union,
    field = "id"
  )
  
  # Mask to only irrigated pixels for each field
  fields_irr_rast = mask(fields_rast, irr_union)
  
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
