
library(tidyverse)
library(sf)
library(httr)
library(furrr)
library(parallel)

# ==== LOAD ====================================================================

# Load raw Points of Diversion
pod_raw = st_read("Data/Raw/Water Rights/Point of Diversion/Utah_Points_of_Diversion.gpkg") |> 
  st_make_valid() |> 
  st_transform(crs = 26912) |> 
  filter(!st_is_empty(SHAPE), WRNUM != "")

# ==== DOWNLOAD POU ============================================================

# Water right numbers to download POU for
wr_nums = sort(unique(pod_raw$WRNUM))

# Function to download a right's place-of-use KML file
download_kml = function(wr, folder = "Data/Raw/Water Rights/Place of Use") {
  url = paste0("https://maps.waterrights.utah.gov/py/RiseAndShout.py?wrName=", URLencode(wr))
  file_path = file.path(folder, paste0(wr, ".kml"))
  
  resp = GET(url)
  
  if (status_code(resp) == 200) {
    writeBin(content(resp, "raw"), file_path)
  } else {
    warning(wr, "failed")
  }
}

# Use all CPU cores for parallel processing
plan(multisession, workers = detectCores())

# Download KML files in parallel
future_map(wr_nums, download_kml)
