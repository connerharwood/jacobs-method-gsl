
library(tidyverse)
library(sf)
library(DescTools)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load GSL subbasins spatial data
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  # Select only subbasin name
  select(subbasin = Name) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load Utah counties spatial data
counties = st_read("Data/Raw/Counties/Counties.shp") |> 
  # Select only county name
  select(county = NAME) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # Filter to counties that intersect GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# Load 2024 WRLU fields spatial data
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |>
  # Filter to 2024 fields
  filter(year == 2024) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # Filter to fields that intersect GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# Load irrigation company service areas
service_areas = st_read("Data/Raw/Service Areas/Irrigation_Company_Service_Areas.shp") |> 
  # Select and rename needed variables
  select(
    `Company Name` = COMPNAME, 
    `Service Acres` = ACRES, 
    County = COUNTY, 
    Basin = BASIN,
    `DWRi Link` = WRLINK,
    `Water Rights` = WATERRGHTS
  ) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # Filter to service areas that intersect GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# Load annual field-level depletion data
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# Filter depletion data to GSL Basin fields
depletion_annual = depletion_annual |> filter(id %in% fields$id)

# ==== LINK FIELDS TO GSL SUBBASINS ============================================

# Find intersection between each field and GSL Basin
fields_basins_intersection = st_intersection(fields, gsl_basin) |> 
  # Calculate area of overlap
  mutate(area_overlap = st_area(geom))

# For each field, keep subbasin with most overlap
fields_basins_match = fields_basins_intersection |> 
  group_by(id) |> 
  slice_max(area_overlap, n = 1, with_ties = FALSE) |> 
  ungroup()

# Join field subbasin back to main fields data
fields_basins = fields |> 
  left_join(
    fields_basins_match |> 
      st_drop_geometry() |> 
      select(id, subbasin),
    by = "id",
    relationship = "one-to-one"
  )

# ==== LINK FIELDS TO SERVICE AREAS ============================================

fields_service_intersection = st_intersection(fields, service_areas) |> 
  mutate(
    # Calculate area of overlap
    area_overlap = as.numeric(st_area(geom)) / 4046.8564224,
    # Calculate percent of field that overlaps with service area
    percent_overlap = area_overlap / acres
  ) |> 
  # For each field, keep the service area it has the most overlap with
  group_by(id) |> 
  slice_max(order_by = percent_overlap, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  # Only keep fields with at least 50% overlap with a service area
  filter(percent_overlap >= 0.5)

fields_service_area = field |> 
  left_join(
    # Join fields with service area if they qualified
    fields_service_intersection |> 
      st_drop_geometry() |> 
      select(
        id,
        `Company Name` = Company.Name,
        `Service Acres` = Service.Acres,
        
      )
  )





# ==== COLLAPSE TO FIELDS CROSS SECTION ========================================

# Create mode function
mode_function = function(x, na.rm = TRUE) {
  if (na.rm) x = x[!is.na(x)]
  if (!length(x)) return(NA)
  
  # Build frequency table
  t = table(x)
  max_freq = max(t)
  tied = names(t)[t == max_freq]
  
  # Return first tied value in order of x
  for (value in x) {
    if (value %in% tied) return(value)
  }
}

# Collapse depletion panel to cross section of fields
arcgis_fields = depletion_annual |> 
  # Select and rename needed variable
  select(
    OBJECTID = id,
    Year = year,
    County = county,
    Basin = basin,
    `Sub Area` = sub_area,
    `Land Use Group` = land_use_group,
    Crop = crop,
    `Crop Group` = crop_group,
    `Irrigation Method` = irr_method,
    Acres = acres,
    `Jacobs Method Depletion (ft)` = depletion_ft
  ) |> 
  # Arrange dataset by field ID and descending year
  arrange(OBJECTID, desc(Year)) |> 
  # Remove 2017 depletion estimates (to use 7-year median up to 2024)
  filter(Year != 2017) |> 
  # Group by field
  group_by(OBJECTID) |> 
  # Collapse to one observation per field, taking either the first or mode observation of each variable, and median for depletion
  summarize(
    County = first(County),
    Basin = first(Basin),
    `Sub Area` = first(`Sub Area`),
    `Land Use Group` = mode_function(`Land Use Group`),
    Crop = mode_function(Crop),
    `Crop Group` = mode_function(`Crop Group`),
    `Irrigation Method` = mode_function(`Irrigation Method`),
    Acres = first(Acres),
    `Jacobs Method Depletion (ft)` = median(`Jacobs Method Depletion (ft)`, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Join each field with its geometry
  left_join(
    fields |> select(id, geom),
    by = c("OBJECTID" = "id"),
    relationship = "one-to-one"
  ) |> 
  st_as_sf()

