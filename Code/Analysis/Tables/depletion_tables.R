
library(tidyverse)
library(sf)
library(stargazer)
library(scales)

comma2 = comma_format(accuracy = 0.01)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  filter(basin != "Strawberry") |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load Utah counties
counties = st_read("Data/Raw/Counties/Counties.shp") |> 
  # Select only county name
  select(county = NAME) |> 
  # Convert county name to title case
  mutate(county = str_to_title(county)) |> 
  # Remove Duchesne County
  filter(county != "Duchesne") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations and plotting
  st_transform(crs = 26912) |> 
  # Filter to counties that intersect GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

# Load irrigation company service areas
companies = st_read("Data/Raw/Service Areas/Irrigation_Company_Service_Areas.shp") |> 
  # Select and rename needed variables
  select(
    company = COMPNAME, 
    service_acres = ACRES, 
    county = COUNTY, 
    basin = BASIN,
    sub_area = SUBAREANAM,
    land_area = LANAME,
    link = WRLINK,
    rights = WATERRGHTS
  ) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations and plotting
  st_transform(crs = 26912) |> 
  mutate(computed_area = st_area(geometry), computed_acres = as.numeric(st_area(geometry)) / 4046.8564224) |> 
  st_intersection(basin_boundary) |> 
  mutate(
    company = case_when(
      company == "South Jordan Canal Co." & county == "Salt Lake" ~ "South Jordan Canal Co. Salt Lake",
      company == "South Jordan Canal Co." & county == "Morgan" ~ "South Jordan Canal Co. Morgan",
      company == "Unknown" & service_acres < 250 ~ "Unknown 1",
      company == "Unknown" & service_acres > 250 ~ "Unknown 2",
      TRUE ~ company
    ),
    overlap_area = st_area(st_intersection(geometry, basin_boundary)),
    overlap_percent = as.numeric(overlap_area / computed_area)
  ) |> 
  filter(overlap_percent >= 0.90)

# Load 2024 WRLU fields
fields = st_read("Data/Clean/fields_panel.gpkg") |> 
  filter(year == 2024) |> 
  st_filter(basin_boundary, .predicate = st_intersects)

# Load pivot corners
pivot_corners = map_dfr(
  st_layers("Data/Clean/pivot_corners.gpkg")$name,
  \(x) st_read("Data/Clean/pivot_corners.gpkg", layer = x),
  .id = "county"
) |> 
  select(id)

# Load field-level annual depletions
load("Data/Clean/depletion_annual.rda")

# ==== PREP ====================================================================

# Filter depletion data to actively irrigated GSL Basin fields
depletion_annual = depletion_annual |> 
  filter(
    year > 2017, 
    id %in% fields$id, 
    crop != "Fallow/Idle",
    land_use_group %in% c("Active IR", "SubIRR", NA)
  )

# Assign fields to companies
fields_company = fields |>
  select(id, acres) |> 
  st_intersection(companies) |> 
  mutate(
    # Calculate area of field and POU intersection
    area_overlap = as.numeric(st_area(geom)) / 4046.8564224,
    # Calculate proportion of overlap between each field and POU polygon
    percent_overlap = area_overlap / acres
  ) |> 
  # Evaluate intersections for each field and year
  group_by(id) |> 
  # For each field and year, keep only the feature with highest overlap with POU
  slice_max(order_by = percent_overlap, n = 1, with_ties = FALSE) |> 
  # Filter to fields with at least 90% overlap with a service area
  filter(percent_overlap >= 0.90)

# Merge field-level depletion with companies
depletion_annual_company = depletion_annual |> 
  left_join(
    fields_company |> 
      st_drop_geometry() |> 
      select(id, company, company_county = county),
    by = "id",
    relationship = "many-to-one"
  )

# Merge pivot corners with annual depletion
corner_depletion_annual = pivot_corners |> 
  left_join(
    depletion_annual,
    by = "id",
    relationship = "one-to-many"
  ) |> 
  filter(
    year > 2017, 
    crop != "Fallow/Idle",
    land_use_group %in% c("Active IR", "SubIRR", NA)
  )

# Identify non-pivot corners, merge with annual depletion
noncorner_depletion_annual = fields |> 
  select(id) |> 
  filter(!id %in% pivot_corners$id) |> 
  left_join(
    depletion_annual,
    by = "id",
    relationship = "one-to-many"
  ) |> 
  filter(
    year > 2017, 
    crop != "Fallow/Idle",
    land_use_group %in% c("Active IR", "SubIRR", NA)
  )

# ==== BY CROP =================================================================

by_crop = depletion_annual |> 
  group_by(crop, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(crop) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(!is.na(crop)) |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 10)

by_crop_table = by_crop |> 
  bind_rows(
    tibble(
      crop = "Total",
      acres = sum(by_crop$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(by_crop$depletion_ft, w = by_crop$acres, na.rm = TRUE),
      depletion_af = sum(by_crop$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> comma(),
    depletion_ft = depletion_ft |> comma2(),
    depletion_af = depletion_af |> comma()
  )

stargazer(
  by_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BY COUNTY ===============================================================

by_county = depletion_annual |> 
  group_by(county, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(county) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), 
    .groups = "drop"
  ) |> 
  arrange(desc(depletion_af))

by_county_table = by_county |> 
  bind_rows(
    tibble(
      county = "Total",
      acres = sum(by_county$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(by_county$depletion_ft, w = by_county$acres, na.rm = TRUE),
      depletion_af = sum(by_county$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> comma(),
    depletion_ft = depletion_ft |> comma2(),
    depletion_af = depletion_af |> comma()
  )

stargazer(
  by_county_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BOX ELDER BY CROP =======================================================

boxelder_by_crop = depletion_annual |> 
  filter(county == "Box Elder") |> 
  group_by(crop, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(crop) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop"
  ) |> 
  filter(!is.na(crop), crop != "Fallow/Idle") |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 11)

boxelder_by_crop_table = boxelder_by_crop |> 
  bind_rows(
    tibble(
      crop = "Total",
      acres = sum(boxelder_by_crop$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(boxelder_by_crop$depletion_ft, w = boxelder_by_crop$acres, na.rm = TRUE),
      depletion_af = sum(boxelder_by_crop$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> comma(),
    depletion_ft = depletion_ft |> comma2(),
    depletion_af = depletion_af |> comma()
  )

stargazer(
  boxelder_by_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== CACHE BY CROP ===========================================================

cache_by_crop = depletion_annual |> 
  filter(county == "Cache") |> 
  group_by(crop, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(crop) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop"
  ) |> 
  filter(!is.na(crop), crop != "Fallow/Idle") |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 11)

cache_by_crop_table = cache_by_crop |> 
  bind_rows(
    tibble(
      crop = "Total",
      acres = sum(cache_by_crop$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(cache_by_crop$depletion_ft, w = cache_by_crop$acres, na.rm = TRUE),
      depletion_af = sum(cache_by_crop$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> comma(),
    depletion_ft = depletion_ft |> comma2(),
    depletion_af = depletion_af |> comma()
  )

stargazer(
  cache_by_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BOX ELDER AND CACHE BY CROP =============================================

boxelder_cache_by_crop = left_join(
  boxelder_by_crop,
  cache_by_crop,
  by = "crop"
) |> 
  select(
    crop,
    acres_boxelder = acres.x,
    acres_cache = acres.y,
    depletion_ft_boxelder = depletion_ft.x,
    depletion_ft_cache = depletion_ft.y,
    depletion_af_boxelder = depletion_af.x,
    depletion_af_cache = depletion_af.y,
  ) |> 
  filter(crop != "Turfgrass Ag")

boxelder_cache_by_crop_table = boxelder_cache_by_crop |> 
  bind_rows(
    tibble(
      crop = "Total",
      acres_boxelder = sum(boxelder_cache_by_crop$acres_boxelder, na.rm = TRUE),
      acres_cache = sum(boxelder_cache_by_crop$acres_cache, na.rm = TRUE),
      depletion_ft_boxelder = weighted.mean(boxelder_cache_by_crop$depletion_ft_boxelder, w = boxelder_cache_by_crop$acres_boxelder, na.rm = TRUE),
      depletion_ft_cache = weighted.mean(boxelder_cache_by_crop$depletion_ft_cache, w = boxelder_cache_by_crop$acres_cache, na.rm = TRUE),
      depletion_af_boxelder = sum(boxelder_cache_by_crop$depletion_af_boxelder, na.rm = TRUE),
      depletion_af_cache = sum(boxelder_cache_by_crop$depletion_af_cache, na.rm = TRUE)
    )
  ) |> 
  mutate(
    acres_boxelder = acres_boxelder |> comma(),
    acres_cache = acres_cache |> comma(),
    depletion_ft_boxelder = depletion_ft_boxelder |> comma2(),
    depletion_ft_cache = depletion_ft_cache |> comma2(),
    depletion_af_boxelder = depletion_af_boxelder |> comma(),
    depletion_af_cache = depletion_af_cache |> comma()
  )

stargazer(
  boxelder_cache_by_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)
  
# ==== BY COUNTY, CORNERS ======================================================

# Calculate median annual depletion depth by county for active fields
by_county_corners = corner_depletion_annual |> 
  st_drop_geometry() |> 
  group_by(county, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(county) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(desc(depletion_af))

by_county_corners_table = by_county_corners |> 
  bind_rows(
    tibble(
      county = "Total",
      acres = sum(by_county_corners$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(by_county_corners$depletion_ft, w = by_county_corners$acres, na.rm = TRUE),
      depletion_af = sum(by_county_corners$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> round(0) |> comma(),
    depletion_ft = depletion_ft |> round(2) |> comma2(),
    depletion_af = depletion_af |> round(0) |> comma()
  )

# ==== BY COUNTY, NON-CORNERS===================================================

# Calculate median annual depletion depth by county for active fields
by_county_noncorners = noncorner_depletion_annual |> 
  st_drop_geometry() |> 
  group_by(county, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(county) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(desc(depletion_af))

by_county_noncorners_table = by_county_noncorners |> 
  bind_rows(
    tibble(
      county = "Total",
      acres = sum(by_county_noncorners$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(by_county_noncorners$depletion_ft, w = by_county_noncorners$acres, na.rm = TRUE),
      depletion_af = sum(by_county_noncorners$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> round(0) |> comma(),
    depletion_ft = depletion_ft |> round(2) |> comma2(),
    depletion_af = depletion_af |> round(0) |> comma()
  )

# ==== BY COUNTY, CORNERS AND NON-CORNERS ======================================

by_county_corners_noncorners = left_join(
  by_county_corners,
  by_county_noncorners,
  by = "county",
  relationship = "one-to-one"
) |> 
  select(
    county,
    acres_corners = acres.x,
    acres_noncorners = acres.y,
    depletion_ft_corners = depletion_ft.x,
    depletion_ft_noncorners = depletion_ft.y,
    depletion_af_corners = depletion_af.x,
    depletion_af_noncorners = depletion_af.y,
  )

by_county_corners_noncorners_table = by_county_corners_noncorners |> 
  bind_rows(
    tibble(
      county = "Total",
      acres_corners = sum(by_county_corners_noncorners$acres_corners, na.rm = TRUE),
      acres_noncorners = sum(by_county_corners_noncorners$acres_noncorners, na.rm = TRUE),
      depletion_ft_corners = weighted.mean(by_county_corners_noncorners$depletion_ft_corners, w = by_county_corners_noncorners$acres_corners, na.rm = TRUE),
      depletion_ft_noncorners = weighted.mean(by_county_corners_noncorners$depletion_ft_noncorners, w = by_county_corners_noncorners$acres_noncorners, na.rm = TRUE),
      depletion_af_corners = sum(by_county_corners_noncorners$depletion_af_corners, na.rm = TRUE),
      depletion_af_noncorners = sum(by_county_corners_noncorners$depletion_af_noncorners, na.rm = TRUE)
    )
  ) |> 
  mutate(
    acres_corners = acres_corners |> round(0) |> comma(),
    acres_noncorners = acres_noncorners |> round(0) |> comma(),
    depletion_ft_corners = depletion_ft_corners |> round(2) |> comma(),
    depletion_ft_noncorners = depletion_ft_noncorners |> round(2) |> comma(),
    depletion_af_corners = depletion_af_corners |> round(0) |> comma(),
    depletion_af_noncorners = depletion_af_noncorners |> round(0) |> comma()
  )

stargazer(
  by_county_corners_noncorners_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:corners_noncorners_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BOX ELDER BY COMPANY ====================================================

boxelder_by_company = depletion_annual_company |> 
  filter(company_county == "Box Elder") |> 
  group_by(company, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(company) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop"
  ) |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 10)

boxelder_by_company_table = boxelder_by_company |> 
  bind_rows(
    tibble(
      company = "Total",
      acres = sum(boxelder_by_company$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(boxelder_by_company$depletion_ft, w = boxelder_by_company$acres, na.rm = TRUE),
      depletion_af = sum(boxelder_by_company$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> comma(),
    depletion_ft = depletion_ft |> comma2(),
    depletion_af = depletion_af |> round(0) |> comma()
  )

stargazer(
  boxelder_by_company_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:boxelder_company_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== CACHE BY COMPANY ========================================================

company_medians = depletion_annual_company |> 
  filter(!is.na(company)) |> 
  group_by(company, company_county, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(company, company_county) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop"
  ) |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 10)

cache_by_company = depletion_annual_company |> 
  filter(company_county == "Cache") |> 
  group_by(company, year) |> 
  mutate(
    acres_total = sum(acres, na.rm = TRUE),
    depletion_af_total = sum(depletion_af, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(company) |> 
  summarize(
    acres = median(acres_total, na.rm = TRUE),
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop"
  ) |> 
  arrange(desc(depletion_af)) |> 
  slice_max(depletion_af, n = 10)

cache_by_company_table = cache_by_company |> 
  bind_rows(
    tibble(
      company = "Total",
      acres = sum(cache_by_company$acres, na.rm = TRUE),
      depletion_ft = weighted.mean(cache_by_company$depletion_ft, w = cache_by_company$acres, na.rm = TRUE),
      depletion_af = sum(cache_by_company$depletion_af, na.rm = TRUE)
    )
  ) |>
  mutate(
    acres = acres |> comma(),
    depletion_ft = depletion_ft |> comma2(),
    depletion_af = depletion_af |> round(0) |> comma()
  )

stargazer(
  cache_by_company_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:cache_company_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BOX ELDER AND CACHE BY COMPANY ==========================================

boxelder_cache_by_company = rbind(boxelder_by_company_table, cache_by_company_table)

stargazer(
  boxelder_cache_by_company,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:boxelder_cache_company_depletion",
  float = TRUE,
  float.env = "table"
)
