
library(tidyverse)
library(sf)
library(httr)
library(furrr)
library(parallel)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load raw Points of Diversion
pod_raw = st_read("Data/Raw/Water Rights/Points of Diversion/Utah_Points_of_Diversion.gpkg") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # Filter to PODs within GSL Basin
  st_filter(gsl_basin, .predicate = st_within) |> 
  # Remove PODs missing geometry or water right number
  filter(!st_is_empty(SHAPE), WRNUM != "")

# ==== DOWNLOAD POU ============================================================

# Water right numbers to download POU for
wr_nums = sort(unique(pod_raw$WRNUM))

# Function to download a right's place-of-use KML file
download_kml = function(wr, folder = "Data/Raw/Water Rights/Places of Use") {
  # Create download URL
  url = paste0("https://maps.waterrights.utah.gov/py/RiseAndShout.py?wrName=", URLencode(wr))
  
  # Create file path for storing downloaded KML
  file_path = file.path(folder, paste0(wr, ".kml"))
  
  # Retrieve URL content (KML file)
  resp = GET(url)
  
  # If retrieval was successful, save KML file to designated path
  if (status_code(resp) == 200) {
    writeBin(content(resp, "raw"), file_path)
  # If retrieval failed, print message and move to next file
  } else {
    warning(wr, "failed")
  }
}

# Use all CPU cores for parallel processing
plan(multisession, workers = detectCores())

# Download KML files in parallel
future_map(wr_nums, download_kml)

# ==== ATTEMPT 2 ===============================================================

wr_downloads = list.files(
  "Data/Raw/Water Rights/Places of Use",
  pattern = "\\.kml$",
  full.names = FALSE
) |> 
  tools::file_path_sans_ext()

missing_wr_nums = setdiff(wr_nums, wr_downloads)

# Use all CPU cores for parallel processing
plan(multisession, workers = detectCores())

# Download KML files in parallel
future_map(missing_wr_nums, download_kml)

