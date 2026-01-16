
library(tidyverse)
library(sf)
library(tools)
library(gdalraster)

# List water right .kml files
kml_files = list.files("Data/Raw/Water Rights/Places of Use", pattern = "\\.kml$", full.names = TRUE)

# Load service area (Place of Use polygons)
pou_raw = map_dfr(kml_files, function(path) {
  # Extract water right number from file name
  wr_num = file_path_sans_ext(basename(path))
  
  tryCatch({
    # Load and filter to polygons (remove points of diversion)
    kml = st_read(path, quiet = TRUE) |> 
      filter(st_geometry_type(geometry) != "POINT") |> 
      mutate(wr_num = wr_num) |> 
      select(wr_num, geometry)
  }, error = function(e) {
    message("Skipping invalid file: ", basename(path))
    file.remove(path)
    NULL
  })
}) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to WGS 84
  st_transform(crs = 26912)

# Combine individual POU polygons for each right into multipolygon
pou = pou_raw |> 
  group_by(wr_num) |> 
  summarize(geometry = st_combine(geometry), .groups = "drop") |> 
  st_make_valid() |> 
  st_buffer(0)

# Load service area (Point of Diversion points)
pod_raw = map_dfr(kml_files, function(path) {
  # Extract water right number from file name
  wr_num = file_path_sans_ext(basename(path))
  
  tryCatch({
    # Load and filter to points (remove places of use)
    kml = st_read(path, quiet = TRUE) |> 
      filter(st_geometry_type(geometry) == "POINT") |> 
      mutate(wr_num = wr_num) |> 
      select(wr_num, geometry)
  }, error = function(e) {
    message("Skipping invalid file: ", basename(path))
    file.remove(path)
    NULL
  })
  
}) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Transform to WGS 84
  st_transform(crs = 26912)

# ==== SAVE ====================================================================

# Create initial geopackage file to be fixed
st_write(pou, "Data/Clean/Water Rights/pou_invalid.gpkg", layer = "pou", delete_dsn = TRUE)
st_write(pod, "Data/Clean/Water Rights/pod_invalid.gpkg", layer = "pod", delete_dsn = TRUE)

# Fix invalid winding orders, save as new gpkg
ogr2ogr(
  src_dsn = "Data/Clean/Water Rights/pou_invalid.gpkg",
  dst_dsn = "Data/Clean/Water Rights/pou.gpkg",
  cl_arg = "-makevalid"
)
ogr2ogr(
  src_dsn = "Data/Clean/Water Rights/pod_invalid.gpkg",
  dst_dsn = "Data/Clean/Water Rights/pod.gpkg",
  cl_arg = "-makevalid"
)

# Remove invalid file
file.remove("Data/Clean/Water Rights/pou_invalid.gpkg")
file.remove("Data/Clean/Water Rights/pod_invalid.gpkg")
