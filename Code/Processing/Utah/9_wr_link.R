
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load WRLU fields panel
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  # Filter to 2024 fields
  filter(year == 2024) |> 
  # Make geometry valid
  st_make_valid()

# Load Place of Use polygons
pou = st_read("Data/Clean/Water Rights/pou.gpkg") |> 
  # Keep only fields that intersect a POU polygon
  st_filter(fields, .predicate = st_intersects) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Add buffer of 0 to fix geometries missed by st_make_valid
  st_buffer(0)

# Load field-level monthly depletion data
load("Data/Clean/Depletion/Utah/depletion_monthly.rda")

# ==== POU-FIELD LINK ==========================================================

# Assign each field to water right it has the most overlap with
overlapping_fields = st_intersection(
  fields,
  pou
) |> 
  # Calculate area of field and POU intersection
  mutate(area_overlap = as.numeric(st_area(geom)) / 4046.8564224) |> 
  # Evaluate intersections for each field and year
  group_by(id) |> 
  # Calculate proportion of overlap between each field and POU polygon
  mutate(percent_overlap = area_overlap / acres) |> 
  # For each field and year, keep only the feature with highest overlap with POU
  slice_max(order_by = percent_overlap, n = 1, with_ties = FALSE) |> 
  # Filter out fields with less 10% overlap with service area
  filter(percent_overlap >= 0.4)

# Identify qualified fields
fields_pou = fields |>
  left_join(
    # Join all fields with qualified fields
    overlapping_fields |> st_drop_geometry() |> select(id, wr_num), 
    by = c("id")
  ) |> 
  # Filter out fields that don't overlap with a water right POU
  filter(!is.na(wr_num)) |> 
  # Make geometry valid
  st_make_valid()

# Inspect field identification within service area
tm_shape(fields_pou) +
  tm_polygons(col = "lightgreen", border.col = "darkgreen", group = "Fields") +
  tm_shape(pou) +
  tm_borders(col = "red", lwd = 2, lty = "dashed", group = "POU") +
  tm_layout(legend.outside = TRUE)

# ==== WR DEPLETION ============================================================

depletion_monthly_wr = overlapping_fields |> 
  st_drop_geometry() |> 
  select(id, wr_num, area_overlap) |> 
  left_join(depletion_monthly, by = "id") |> 
  group_by(wr_num, year, month) |> 
  summarize(
    acres_overlap = sum(area_overlap),
    depletion_ft = weighted.mean(depletion_ft, w = area_overlap, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(depletion_af = depletion_ft * acres_overlap)

annual = depletion_monthly_wr |> 
  group_by(wr_num, year) |> 
  summarize(
    depletion_ft = sum(depletion_ft, na.rm = FALSE), 
    depletion_af = sum(depletion_af, na.rm = FALSE),
    .groups = "drop"
  )

median(annual$depletion_af, na.rm = TRUE)
