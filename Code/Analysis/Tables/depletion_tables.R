
library(tidyverse)
library(sf)
library(stargazer)
library(scales)

comma2 = comma_format(accuracy = 0.01)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load Utah counties
counties = st_read("Data/Raw/Counties/Counties.shp") |> 
  # Select only county name
  select(county = NAME) |> 
  # Convert county name to title case
  mutate(county = str_to_title(county)) |> 
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
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  filter(year == 2024) |> 
  st_filter(gsl_basin, .predicate = st_intersects)

# Load field-level annual depletions
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# Filter depletion data to GSL Basin fields
depletion_annual = depletion_annual |> 
  filter(year > 2017, id %in% fields$id, crop != "Fallow/Idle")

# ==== PREP ====================================================================

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

depletion_annual_company = depletion_annual |> 
  left_join(
    fields_company |> 
      st_drop_geometry() |> 
      select(id, company, company_county = county),
    by = "id",
    relationship = "many-to-one"
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

# ==== BY RIVER BASIN ==========================================================

compare = read_csv("depletion_comparison.csv")

basin_order <- compare |>
  group_by(basin) |>
  summarize(rank_val = mean(depletion_jm, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(rank_val)) |>
  pull(basin)

compare <- compare |>
  mutate(basin = factor(basin, levels = basin_order))

comparison_plot = ggplot(compare, aes(x = year)) +
  geom_line(aes(y = depletion_jm, color = "Jacobs Method")) +
  geom_line(aes(y = depletion_wb, color = "Water Budget")) +
  facet_wrap(~ basin, scales = "free_x", ncol = 3) +
  scale_x_continuous(breaks = c(2018:2024)) +
  scale_y_continuous(breaks = seq(from = 0, to = 600000, by = 100000)) +
  #theme_minimal() +
  labs(
    title = "Jacobs Method vs. Water Budget Ag Depletion",
    x = "Year",
    y = "Depletion Volume (AF)",
    color = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "black"),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(angle = 50, vjust = 0.7),
    axis.line = element_line(),
    #panel.grid.major = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
  )
comparison_plot

ggsave(
  "depletion_comparison.png",
  plot = comparison_plot,
  height = 8,
  width = 14,
  dpi = 300
)

totals = compare |> 
  group_by(year) |> 
  summarize(depletion_jm = sum(depletion_jm), depletion_wb = sum(depletion_wb), .groups = "drop")

sby_basin_annual = depletion_annual |> 
  group_by(basin, year) |> 
  summarize(depletion_af = sum(depletion_af, na.rm = TRUE), .groups = "drop") |> 
  filter(!basin %in% c("Sevier River", "Uintah")) |> 
  rename(depletion_jm = depletion_af) |> 
  mutate(depletion_wb = NA)

write_csv(by_basin_annual, file = "depletion_comparison.csv")

by_basin = depletion_annual |> 
  group_by(basin, year) |> 
  mutate(depletion_af_total = sum(depletion_af, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(basin) |> 
  summarize(depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop") #|> 
  pivot_wider(names_from = basin, values_from = depletion_af)# |> 
  #filter(crop %in% by_crop$crop) |> 
  #arrange(factor(crop, levels = by_crop$crop))


try = depletion_annual |> 
  
by_basin_crop_table = by_basin_crop |> 
  bind_rows(
    summarise(
      by_basin_crop,
      crop = "Total",
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
    )
  ) |> 
  mutate(
    across(where(is.numeric), ~ round(.x, 2)),
    across(where(is.numeric), ~ comma2(.x))
  )

stargazer(
  by_basin_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BY RIVER BASIN AND CROP =================================================

by_basin_crop = depletion_annual |> 
  group_by(basin, crop, year) |> 
  mutate(depletion_af_total = sum(depletion_af, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(basin, crop) |> 
  summarize(depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = basin, values_from = depletion_af) |> 
  filter(crop %in% by_crop$crop) |> 
  arrange(factor(crop, levels = by_crop$crop))

by_basin_crop_table = by_basin_crop |> 
  bind_rows(
    summarise(
      by_basin_crop,
      crop = "Total",
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
    )
  ) |> 
  mutate(
    across(where(is.numeric), ~ round(.x, 2)),
    across(where(is.numeric), ~ comma2(.x))
  )

stargazer(
  by_basin_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)

# ==== BY COUNTY AND CROP ======================================================

by_county_crop = depletion_annual |> 
  group_by(county, crop, year) |> 
  mutate(depletion_af_total = sum(depletion_af, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(county, crop) |> 
  summarize(depletion_af = median(depletion_af_total, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = county, values_from = depletion_af) |> 
  filter(crop %in% by_crop$crop) |> 
  arrange(factor(crop, levels = by_crop$crop))

by_county_crop_table = by_county_crop |> 
  bind_rows(
    summarise(
      by_county_crop,
      crop = "Total",
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
    )
  ) |> 
  mutate(
    across(where(is.numeric), ~ round(.x, 2)),
    across(where(is.numeric), ~ comma2(.x))
  )

stargazer(
  by_county_crop_table,
  summary = FALSE,
  rownames = FALSE,
  label = "tab:crop_depletion",
  float = TRUE,
  float.env = "table"
)
