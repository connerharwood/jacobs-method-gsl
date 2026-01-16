
library(tidyverse)
library(sf)
library(tools)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# List water right .kml files
#kml_files = list.files("Data/GSLCO/KML Files", pattern = "\\.kml$", full.names = TRUE)
kml_files = list.files("Data/Raw/Water Rights/KML Files/Historic Monastery Farm", pattern = "\\.kml$", full.names = TRUE)

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

# Load 2024 WRLU fields spatial data
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |>
  # Filter to 2024 fields
  filter(year == 2024) |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # Filter to fields that intersect service areas
  st_filter(pou_raw, .predicate = st_intersects)

# Load monthly field-level depletion
load("Data/Clean/Depletion/Utah/depletion_monthly.rda")

# ==== LINK FIELDS TO WATER RIGHT POU ==========================================

# Combine individual POU polygons for each right into multipolygon
pou = pou_raw |> 
  group_by(wr_num) |> 
  summarize(geometry = st_combine(geometry), .groups = "drop") |> 
  st_make_valid()

# Assign each field to water right it has the most overlap with
overlapping_fields = st_intersection(
  fields,
  pou
) |> 
  mutate(
    # Calculate area of field and POU intersection
    area_overlap = as.numeric(st_area(geom)) / 4046.8564224,
    # Calculate proportion of overlap between each field and POU polygon
    percent_overlap = area_overlap / acres
  ) |> 
  # Evaluate intersections for each field and year
  group_by(id) |> 
  # For each field and year, keep only the feature with highest overlap with POU
  slice_max(order_by = percent_overlap, n = 1, with_ties = FALSE) #|> 
  # Filter out fields with less 50% overlap with service area
  filter(percent_overlap >= 0.5)

# Inspect field identification within service area
tm_shape(overlapping_fields) +
  tm_polygons(col = "lightgreen", border.col = "darkgreen", group = "Fields") +
  tm_shape(pou) +
  tm_borders(col = "red", lwd = 2, lty = "dashed", group = "POU") +
  tm_layout(legend.outside = TRUE)

# ==== WR DEPLETION ============================================================

# Subset monthly depletion data to target fields
depletion_monthly_subset = depletion_monthly |> 
  filter(id %in% overlapping_fields$id, year != 2017) |> 
  left_join(
    overlapping_fields |> 
      st_drop_geometry() |> 
      select(id, wr_num, area_overlap),
    by = "id",
    relationship = "many-to-one"
  )# |> 
  # mutate(company = case_when(
  #   wr_num == "25-3514" ~ "Logan River and Blacksmith Fork Irrigation Company",
  #   wr_num == "25-4529" ~ "Spring Creek Cache Irrigation Company"
  # ))

# Calculate irrigation company-level monthly depletions
company_depletion_monthly = depletion_monthly_subset |> 
  group_by(wr_num, year, month) |> 
  summarize(depletion_ft = weighted.mean(depletion_ft, w = area_overlap, na.rm = TRUE), .groups = "drop")

# Calculate irrigation company-level annual depletions
company_depletion_annual = company_depletion_monthly |> 
  group_by(wr_num, year) |> 
  summarize(depletion_ft = sum(depletion_ft, na.rm = FALSE), .groups = "drop")


# Calculate median monthly depletion for each company
median_monthly = company_depletion_monthly |> 
  group_by(wr_num, period = month) |> 
  summarize(depletion_ft = median(depletion_ft, na.rm = TRUE), .groups = "drop") |> 
  mutate(month = month(period, label = TRUE, abbr = FALSE))

# Calculate median annual depletion for each company
median_annual = company_depletion_annual |> 
  group_by(wr_num) |> 
  summarize(depletion_ft = median(depletion_ft, na.rm = TRUE), .groups = "drop") |> 
  mutate(period = "Annual")




compute_median_annual <- function(threshold) {
  
  # Filter fields at this threshold
  overlapping_fields_thr = st_intersection(fields, pou) |> 
    mutate(
      area_overlap = as.numeric(st_area(geom)) / 4046.8564224,
      percent_overlap = area_overlap / acres
    ) |> 
    group_by(id) |> 
    slice_max(order_by = percent_overlap, n = 1, with_ties = FALSE) |> 
    filter(percent_overlap >= threshold)
  
  # Monthly depletion subset
  depletion_monthly_subset = depletion_monthly |> 
    filter(id %in% overlapping_fields_thr$id) |> 
    left_join(
      overlapping_fields_thr |> st_drop_geometry() |> select(id, wr_num, area_overlap),
      by = "id",
      relationship = "many-to-one"
    ) |> 
    mutate(`Irrigation Company` = case_when(
      wr_num == "25-3514" ~ "Logan River and Blacksmith Fork Irrigation Company",
      wr_num == "25-4529" ~ "Spring Creek Cache Irrigation Company"
    ))
  
  # Company-level monthly
  company_depletion_monthly = depletion_monthly_subset |> 
    group_by(`Irrigation Company`, Year = year, Month = month) |> 
    summarize(depletion_ft = weighted.mean(depletion_ft, w = area_overlap, na.rm = TRUE),
              .groups = "drop")
  
  # Annual totals
  company_depletion_annual = company_depletion_monthly |> 
    group_by(`Irrigation Company`, Year) |> 
    summarize(depletion_ft = sum(depletion_ft), .groups = "drop")
  
  # Median annual depletion
  median_annual = company_depletion_annual |> 
    group_by(`Irrigation Company`) |> 
    summarize(depletion_ft = median(depletion_ft), .groups = "drop") |> 
    mutate(threshold = threshold)
  
  return(median_annual)
}

thresholds = seq(0, 1, by = 0.1)

results = map_dfr(thresholds, compute_median_annual)

ggplot(results, aes(x = threshold, y = depletion_ft, color = `Irrigation Company`)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Sensitivity of Annual Median Depletion to Percent-Overlap Threshold",
    x = "Minimum Percent Overlap Required",
    y = "Median Annual Depletion (ft)"
  ) +
  theme_minimal(base_size = 14)

