
library(tidyverse)
library(sf)
library(ggspatial)
library(terra)
library(maptiles)
library(stargazer)
library(scales)
library(tmap)
tmap_mode("view")

comma2 = comma_format(accuracy = 0.01)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  select(basin = Name) |> 
  filter(basin != "Strawberry") |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# Load GSL Basin counties
counties = st_read("Data/Raw/Counties/Counties.shp") |> 
  st_make_valid() |> 
  st_transform(crs = 26912) |> 
  st_filter(gsl_basin, .predicate = st_intersects)

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

# Load 2024 WRLU fields
fields = st_read("Data/Clean/fields_panel.gpkg") |> 
  filter(year == 2024) |> 
  st_filter(gsl_basin, .predicate = st_intersects)

# Load each county's pivot corners
box_elder_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Box Elder")
cache_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Cache")
# None in Carbon County
davis_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Davis")
# None in Duchesne County
juab_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Juab")
morgan_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Morgan")
rich_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Rich")
salt_lake_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Salt Lake")
# None in Sanpete County
summit_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Summit")
tooele_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Tooele")
utah_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Utah")
wasatch_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Wasatch")
weber_corners = st_read("Data/Clean/pivot_corners.gpkg", layer = "Weber")

# Load annual field-level depletion data
load("Data/Clean/depletion_annual.rda")

# ==== PREP ====================================================================

# Combine Cache and Box Elder corners into one sf object
pivot_corners = rbind(
  box_elder_corners,
  cache_corners,
  davis_corners,
  juab_corners,
  morgan_corners,
  rich_corners,
  salt_lake_corners,
  summit_corners,
  tooele_corners,
  utah_corners,
  wasatch_corners,
  weber_corners
) |> 
  select(id) |> 
  st_transform(crs = 26912)

corner_depletion_annual = pivot_corners |> 
  left_join(
    depletion_annual,
    by = "id",
    relationship = "one-to-many"
  ) |> 
  filter(year > 2017, !land_use_group %in% c("Dry Ag", "Idle/Fallow", NA))

noncorner_depletion_annual = fields |> 
  select(id) |> 
  filter(!id %in% pivot_corners$id) |> 
  left_join(
    depletion_annual,
    by = "id",
    relationship = "one-to-many"
  ) |> 
  filter(year > 2017, crop != "Fallow/Idle")

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

# ==== MAP =====================================================================

# Fetch satellite imagery tile for GSL Basin basemap
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldGrayCanvas",
  zoom = 10,
  crop = TRUE,
  project = TRUE
)

# Get spatial extent of tile
basin_tile_extent = ext(basin_tile)

# Convert tile to polygon then sf object
basin_tile_poly = as.polygons(basin_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(basin_tile))

# Transform Utah portion of GSL Basin to align with tile's CRS, convert to terra vector
basin_vect = st_transform(gsl_basin |> st_intersection(counties), crs(basin_tile)) |> vect()

# Crop and mask satellite imagery tile to GSL Basin
basin_tile_crop = crop(basin_tile, basin_vect)
basin_tile_mask = mask(basin_tile_crop, basin_vect)

# Choropleth of field-level median annual depletion depth in GSL Basin
corners_map = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = pivot_corners,
    fill = NA,
    color = "red"
  ) +
  geom_sf(
    data = basin_boundary,
    fill = NA,
    color = "black",
    lwd = 0.2
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, GSL Basin") +
  # Minimalist ggplot theme
  theme_minimal() +
  # Customize plot elements
  theme(
    panel.grid.major = element_blank(), # Remove major panel grids
    panel.grid.minor = element_blank(), # Remove minor panel grids
    axis.text = element_blank(), # Remove axes text
    axis.ticks = element_blank(), # Remove axes ticks
    axis.title = element_blank(), # Remove axes titles
    panel.background = element_rect(fill = "white", color = NA), # Create white background
    plot.background = element_rect(fill = "white", color = NA), # Create white background
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
  )
corners_map

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/pivot_corners.png",
  plot = corners_map,
  width = 16,
  height = 10,
  units = "in",
  dpi = 400
)
