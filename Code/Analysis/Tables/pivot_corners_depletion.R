
library(tidyverse)
library(sf)
library(stargazer)
library(scales)

# library(ggspatial)
# library(prettymapr)
# library(tmap)
# tmap_mode("view")

comma2 = comma_format(accuracy = 0.01)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load 2024 WRLU fields
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  filter(year == 2024) |> 
  st_filter(gsl_basin, .predicate = st_intersects)

# Load each county's pivot corners
box_elder_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Box Elder")
cache_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Cache")
davis_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Davis")
duchesne_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Duchesne")
juab_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Juab")
morgan_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Morgan")
rich_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Rich")
salt_lake_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Salt Lake")
sanpete_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Sanpete")
summit_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Summit")
tooele_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Tooele")
utah_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Utah")
wasatch_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Wasatch")
weber_corners = st_read("Data/Clean/Fields/Utah/pivot_corners.gpkg", layer = "Weber")

# Load annual field-level depletion data
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# Filter depletion data to GSL Basin fields
depletion_annual = depletion_annual |> 
  filter(year > 2017, id %in% fields$id, crop != "Fallow/Idle")

# ==== PREP ====================================================================

# Combine Cache and Box Elder corners into one sf object
pivot_corners = rbind(
  box_elder_corners,
  cache_corners,
  davis_corners,
  duchesne_corners,
  juab_corners,
  morgan_corners,
  rich_corners,
  salt_lake_corners,
  sanpete_corners,
  summit_corners,
  tooele_corners,
  utah_corners,
  wasatch_corners,
  weber_corners
) |> 
  select(id)

# tm_shape(pivot_corners) +
#   tm_polygons(col = "red", fill_alpha = 0.3) +
#   tm_basemap("Esri.WorldImagery")

# Pivot corners depletion
corner_depletion_annual = depletion_annual |> 
  filter(id %in% pivot_corners$id, year != 2017)

# Non-pivot corners depletion
noncorner_depletion_annual = depletion_annual |> 
  filter(!id %in% pivot_corners$id, year != 2017)

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

# ==== BY COUNTY, NON-CORNERS ==================================================

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

# ==== BY COUNTY-CROP MEDIANS, CORNERS =========================================

# Calculate median annual depletion depth by county and crop
county_crop_median_depth = pivot_annual |> 
  st_drop_geometry() |> 
  group_by(county, crop) |> 
  summarize(depletion_ft = median(depletion_ft, na.rm = TRUE), .groups = "drop")

# Calculate median annual depletion volume by county and crop
county_crop_median_volume = pivot_annual |> 
  st_drop_geometry() |> 
  group_by(county, crop, year) |> 
  summarize(depletion_af = sum(depletion_af, na.rm = TRUE), .groups = "drop") |> 
  group_by(county, crop) |> 
  summarize(depletion_af = median(depletion_af), .groups = "drop")

# Merge depth and volume medians into one df
county_crop_median = left_join(
  county_crop_median_depth,
  county_crop_median_volume,
  by = c("county", "crop")
) |> 
  arrange(desc(county), crop)


# ==== NON-CORNER FIELDS DEPLETION =============================================

non_corners = fields |> 
  filter(!id %in% pivot_corners$id)

fields_depletion = non_corners |> 
  select(id, county, basin, sub_area, land_use, acres) |> 
  left_join(
    depletion_annual |> 
      select(id, year, land_use_group, crop, crop_group, irr_method, depletion_ft, depletion_af) |> 
      # Remove 2017 to keep only last 7 years of data
      filter(year != 2017),
    by = "id",
    relationship = "one-to-many"
  )

# Calculate median annual depletion depth by county for active fields
county_active_fields_median_depth = fields_depletion |> 
  filter(crop != "Fallow/Idle") |> 
  st_drop_geometry() |> 
  group_by(county) |> 
  summarize(depletion_ft = median(depletion_ft, na.rm = TRUE), .groups = "drop")

# Calculate median annual depletion volume by county for active fields
county_active_fields_median_volume = fields_depletion |> 
  filter(crop != "Fallow/Idle") |> 
  st_drop_geometry() |> 
  group_by(county, year) |> 
  summarize(depletion_af = sum(depletion_af, na.rm = TRUE), .groups = "drop") |> 
  group_by(county) |> 
  summarize(depletion_af = median(depletion_af), .groups = "drop")

# Merge depth and volume medians into one df
county_active_fields_median = left_join(
  county_active_fields_median_depth,
  county_active_fields_median_volume,
  by = "county"
)

# ==== MERGED TABLE ============================================================

merged_table = left_join(
  county_active_median,
  acreage_by_county,
  by = "county"
) |> 
  rename(
    County = county,
    `Depletion Depth (AFA)` = depletion_ft,
    `Depletion Volume (AF)` = depletion_af,
    `Total Pivot Corner Acres` = total_acres
  ) |> 
  mutate(across(where(is.numeric), ~round(.x, 3)))
  
# ==== PLOT ====================================================================

corners_3857 = st_transform(pivot_corners, 3857)

corners_plot = ggplot() +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(data = corners_3857, fill = NA, color = "red", lwd = 0.3) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
corners_plot

ggsave(
  plot = corners_plot,
  filename = "corners_plot.png",
  width = 12,
  height = 7.3,
  dpi = 300
)
