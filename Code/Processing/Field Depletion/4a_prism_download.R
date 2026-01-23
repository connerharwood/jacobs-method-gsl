
library(tidyverse)

# Directory to download and extract PRISM data to
dir = "Data/Raw/PRISM/"

# Define the months to download for each year
year_months = list(
  "2016" = 11:12,
  "2017" = 1:12,
  "2018" = 1:12,
  "2019" = 1:12,
  "2020" = 1:12,
  "2021" = 1:12,
  "2022" = 1:12,
  "2023" = 1:12,
  "2024" = 1:10
)

# Loop through each year and month
for (year in names(year_months)) {
  for (month in year_months[[year]]) {
    
    # Format month with leading zero
    month_str = sprintf("%02d", month)
    
    # Create download URL
    url = paste0(
      "https://data.prism.oregonstate.edu/time_series/us/an/800m/ppt/monthly/",
      year, "/prism_ppt_us_30s_", year, month_str, ".zip"
    )
    
    dest_zip = paste0(dir, "temp.zip")
    
    # Try downloading until successful
    success = FALSE
    attempt = 1
    while (!success && attempt <= 5) { # Up to 5 attempts
      message("Downloading: ", url, " (attempt ", attempt, ")")
      tryCatch({
        download.file(url, dest_zip, quiet = TRUE, mode = "wb")
        
        # Check if file downloaded
        if (file.exists(dest_zip) && file.info(dest_zip)$size > 0) {
          success = TRUE
        } else {
          Sys.sleep(2) # Wait before retry
        }
      }, error = function(e) {
        message("Download failed: ", conditionMessage(e))
        Sys.sleep(2) # Wait before retry
      })
      attempt = attempt + 1
    }
    
    if (!success) {
      message("Skipping ", year, "-", month_str, ": could not download.")
      next
    }
    
    # Extract downloaded zip
    unzip(dest_zip, exdir = dir)
    
    # Remove temporary zip file
    file.remove(dest_zip)
    
    # Remove any extracted non-TIF files
    all_files = list.files(dir, full.names = TRUE)
    non_tif_files = all_files[!grepl("\\.tif$", all_files, ignore.case = TRUE)]
    file.remove(non_tif_files)
  }
}
