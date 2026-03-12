
library(tidyverse)
library(ggspatial)
library(shadowtext)
library(terra)
library(sf)
library(maptiles)
library(extrafont)

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  # Select only basin name
  select(basin = Name) |> 
  # Remove Strawberry Basin
  filter(basin != "Strawberry") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations and plotting
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
  st_filter(gsl_basin, .predicate = st_intersects) |> 
  # Remove Duchesne County
  filter(county != "Duchesne")

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

# Load WRLU ag fields
fields = st_read("Data/Clean/fields_panel.gpkg") |> 
  # Filter to just 2024 cross section
  filter(year == 2024) |> 
  # Filter to fields that intersect GSL Basin
  st_filter(basin_boundary, .predicate = st_intersects)

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

# Load field-level annual depletions
load("Data/Clean/depletion_annual.rda")

# Filter to active fields in GSL Basin
depletion_annual = depletion_annual |> 
  filter(
    year > 2017, 
    id %in% fields$id, 
    crop != "Fallow/Idle",
    land_use_group %in% c("Active IR", "SubIRR", NA)
  )

# ==== PREP ====================================================================

fields_company = fields |> 
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

company_depletion = depletion_annual |> 
  left_join(
    fields_company |> 
      st_drop_geometry() |> 
      select(id, company),
    by = "id",
    relationship = "many-to-one"
  ) |> 
  group_by(year, company) |> 
  mutate(depletion_af = sum(depletion_af, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(company) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  right_join(
    companies,
    by = "company",
    relationship = "one-to-many"
  ) |> 
  st_as_sf() |> 
  mutate(depletion_af2 = depletion_ft * service_acres)

# ==== COMPANIES DEPTH CHORO ===================================================

# Fetch satellite imagery tile for GSL Basin basemap
basin_tile = get_tiles(
  basin_boundary,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)

# Esri.WorldImagery zoom = 11
# Esri.WorldTopoMap zoom = 10
# Esri.WorldGrayCanvas zoom = 11

# Get spatial extent of tile
basin_tile_extent = ext(basin_tile)

# Convert tile to polygon then sf object
basin_tile_poly = as.polygons(basin_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(basin_tile))

# Transform Utah portion of GSL Basin to align with tile's CRS, convert to terra vector
basin_vect = st_transform(gsl_basin |> st_intersection(counties), crs(basin_tile)) |> vect()

# Crop and mask satellite imagery tile to GSL Basin
basin_tile_crop = crop(basin_tile, basin_vect)
basin_tile_mask = mask(basin_tile_crop, basin_vect)

# Manually designate where to label county names on map
county_labels_company = counties |> 
  st_point_on_surface() |> 
  # Grab X and Y coordinates
  st_coordinates() |>
  # Convert to df
  as.data.frame() |> 
  # Join with county name
  cbind(county = counties$county) |> 
  # Remove county labels for certain counties
  filter(!county %in% c("Sanpete", "Carbon")) |> 
  # Manually nudge location of each county label
  mutate(
    nudge_x = case_when(
      county == "Box Elder" ~ 30000, # Move right
      county == "Davis" ~ 9000, # Move right
      county == "Summit" ~ -30000, # Move left
      county == "Tooele" ~ 50000, # Move right
      county == "Juab" ~ 90000, # Move right
      county == "Duchesne" ~ -18000, # Move left
      TRUE ~ 0
    ),
    nudge_y = case_when(
      county == "Box Elder" ~ 32000, # Move up
      county == "Davis" ~ 13000, # Move up
      county == "Summit" ~ -5000, # Move down
      county == "Morgan" ~ 5000, # Move up
      county == "Weber" ~ 2000, # Move up
      county == "Duchesne" ~ -20000, # Move down
      county == "Juab" ~ 9000, # Move up
      county == "Cache" ~ 2000, # Move up
      TRUE ~ 0
    )
  )

# Choropleth of field-level median annual depletion depth in GSL Basin
company_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = company_depletion, 
    aes(fill = depletion_ft), 
    color = "grey",
    linewidth = 0.01
  ) +
  # Add county boundaries
  geom_sf(
    data = counties |> st_intersection(basin_boundary),
    color = "black",
    fill = NA,
    linewidth = 0.3
  ) +
  # # Add GSL
  # geom_sf(
  #   data = huc12s |> filter(huc12_name == "Great Salt Lake"),
  #   color = "black",
  #   fill = "white",
  #   linewidth = 0.3
  # ) +
  # Create outlined label for GSL
  # geom_shadowtext(
  #   data = huc12s |> 
  #     filter(huc12_name == "Great Salt Lake") |> 
  #     st_centroid() |> 
  #     st_coordinates() |> 
  #     as.data.frame() |> 
  #     mutate(basin = "Great Salt Lake"),
  #   aes(x = X, y = Y, label = basin),
  #   color = "black",
  #   bg.color = "white",
  #   bg.r = 0.1,
  #   size = 4.5,
  #   fontface = "bold",
  #   inherit.aes = FALSE,
  #   nudge_x = 3000,
  #   nudge_y = -8000
  # ) +
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels_company,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.12,
    size = 4.5, 
    fontface = "bold",
    inherit.aes = FALSE,
    nudge_x = county_labels_company$nudge_x,
    nudge_y = county_labels_company$nudge_y
  ) +
  # Add GSL Basin boundary
  # geom_sf(
  #   data = basin_boundary,
  #   color = "grey",
  #   fill = NA,
  #   linewidth = 0.2
  # ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(company_depletion$depletion_ft, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(company_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0.06",
      # "0.8",
      # "1.6",
      # "2.4",
      "2.40"
    ),
    limits = c(
      min(company_depletion$depletion_ft, na.rm = TRUE),
      max(company_depletion$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Company-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    legend.text = element_text(size = 12), # Adjust legend tick labels
    # plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
    plot.title = element_blank(),
    legend.position = c(0.84, 0.9), # Adjust legend position
    legend.justification = c(0, 1) # Adjust legend position
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
company_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/company_depth_choro.png",
  plot = company_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 400
)

# ==== COMPANIES VOLUME CHORO ==================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
company_volume_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = company_depletion, 
    aes(fill = depletion_af), 
    color = "grey",
    linewidth = 0.01
  ) +
  # Add county boundaries
  geom_sf(
    data = counties |> st_intersection(basin_boundary),
    color = "black",
    fill = NA,
    linewidth = 0.3
  ) +
  # # Add GSL
  # geom_sf(
  #   data = huc12s |> filter(huc12_name == "Great Salt Lake"),
  #   color = "black",
  #   fill = "white",
  #   linewidth = 0.3
  # ) +
  # Create outlined label for GSL
  # geom_shadowtext(
  #   data = huc12s |> 
  #     filter(huc12_name == "Great Salt Lake") |> 
  #     st_centroid() |> 
  #     st_coordinates() |> 
  #     as.data.frame() |> 
  #     mutate(basin = "Great Salt Lake"),
  #   aes(x = X, y = Y, label = basin),
  #   color = "black",
  #   bg.color = "white",
  #   bg.r = 0.1,
  #   size = 4.5,
  #   fontface = "bold",
  #   inherit.aes = FALSE,
  #   nudge_x = 3000,
  #   nudge_y = -8000
  # ) +
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels_company,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.12,
    size = 4.5, 
    fontface = "bold",
    inherit.aes = FALSE,
    nudge_x = county_labels_company$nudge_x,
    nudge_y = county_labels_company$nudge_y
  ) +
  # Add GSL Basin boundary
  # geom_sf(
  #   data = basin_boundary,
  #   color = "grey",
  #   fill = NA,
  #   linewidth = 0.2
  # ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(company_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(company_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "109393"
    ),
    limits = c(
      min(company_depletion$depletion_af, na.rm = TRUE),
      max(company_depletion$depletion_af, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Company-Level Median Annual Depletion Volume, GSL Basin", fill = "Depletion (AF)") +
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
    legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    legend.text = element_text(size = 12), # Adjust legend tick labels
    # plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
    plot.title = element_blank(),
    legend.position = c(0.84, 0.9), # Adjust legend position
    legend.justification = c(0, 1) # Adjust legend position
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
company_volume_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/company_volume_choro.png",
  plot = company_volume_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 400
)

# ==== CACHE COMPANIES DEPTH CHORO ======================================================

# Filter to top 10 Cache companies by volume
cache_companies = company_depletion |> 
  filter(county == "Cache") |> 
  slice_max(order_by = depletion_af, n = 10)

# Manually designate where to label county names on map
cache_companies_labels = cache_companies |> 
  st_point_on_surface() |> 
  # Grab X and Y coordinates
  st_coordinates() |>
  # Convert to df
  as.data.frame() |> 
  # Join with county name
  cbind(company = cache_companies$company) |> 
  # Manually nudge location of each county label
  mutate(
    nudge_x = case_when(
      company == "South Jordan Canal Co. Cache" ~ 0, # Move right
      company == "West Cache Irrigation Co." ~ 0, # Move right
      company == "Richmond Irrigation Co." ~ 0, # Move left
      company == "Wellsville-Mendon Conservancy District" ~ 0, # Move right
      company == "Logan & Northern Irrigation Co." ~ 0, # Move right
      company == "Logan River Blacksmith Fork Irrigation Co." ~ 0, # Move left
      company == "US Bureau Of Reclamation North" ~ 0, # Move left
      company == "Nibley Blacksmith Fork Irrigation Co." ~ 0, # Move left
      company == "Wellsville East Field Irrigation And Canal Co." ~ 0, # Move left
      company == "Benson Irrigation Co." ~ 0, # Move left
      TRUE ~ 0
    ),
    nudge_y = case_when(
      company == "South Jordan Canal Co. Cache" ~ 0, # Move right
      company == "West Cache Irrigation Co." ~ 0, # Move right
      company == "Richmond Irrigation Co." ~ 0, # Move left
      company == "Wellsville-Mendon Conservancy District" ~ 0, # Move right
      company == "Logan & Northern Irrigation Co." ~ 0, # Move right
      company == "Logan River Blacksmith Fork Irrigation Co." ~ 0, # Move left
      company == "US Bureau Of Reclamation North" ~ 0, # Move left
      company == "Nibley Blacksmith Fork Irrigation Co." ~ 0, # Move left
      company == "Wellsville East Field Irrigation And Canal Co." ~ 0, # Move left
      company == "Benson Irrigation Co." ~ 0, # Move left
      TRUE ~ 0
    ),
    label_x = X + nudge_x,
    label_y = Y + nudge_y
  )

# Filter county spatial data to Cache County
cache = counties |> filter(county == "Cache")

# Fetch satellite imagery tile for Cache County basemap
cache_tile = get_tiles(
  cache,
  provider = "Esri.WorldImagery",
  zoom = 14,
  crop = TRUE,
  project = TRUE
)

# Esri.WorldImagery zoom = 14
# Esri.WorldTopoMap zoom = 11
# Esri.WorldGrayCanvas zoom = 11

# Get spatial extent of tile
cache_tile_extent = ext(cache_tile)

# Convert tile to polygon then sf object
cache_tile_poly = as.polygons(cache_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(cache_tile))

# Transform Cache County to align with tile's CRS, convert to terra vector
cache_vect = st_transform(cache, crs(cache_tile)) |> vect()

# Crop and mask satellite imagery tile to Cache County
cache_tile_crop = crop(cache_tile, cache_vect)
cache_tile_mask = mask(cache_tile_crop, cache_vect)

# Choropleth of field-level median annual depletion depth in Cache County
cache_companies_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = cache_tile_mask) +
  # Color code each Cache County field by median annual depletion depth
  geom_sf(
    data = cache_companies,
    aes(fill = depletion_ft),
    color = NA
  ) +
  geom_segment(
    data = cache_companies_labels,
    aes(
      x = X, y = Y,
      xend = label_x, yend = label_y
    ),
    arrow = arrow(length = unit(0.12, "in"), type = "closed"),
    linewidth = 0.5,
    color = "black",
    inherit.aes = FALSE
  ) +
  # Create outlined labels for each county
  geom_shadowtext(
    data = cache_companies_labels,
    aes(
      x = label_x,
      y = label_y,
      label = company
    ),
    color = "black",
    bg.color = "white",
    bg.r = 0.15,
    size = 4.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(cache_companies$depletion_ft, na.rm = TRUE),
      #0.8, 1.6, 2.4,
      max(cache_companies$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "3.19"
    ),
    limits = c(
      min(cache_companies$depletion_ft, na.rm = TRUE),
      max(cache_companies$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Company-Level Median Annual Depletion Depth, Cache County", fill = "Depletion (AFA)") +
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
    legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    legend.text = element_text(size = 12), # Adjust legend tick labels
    plot.title = element_blank() # Adjust plot title
    #plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
cache_companies_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/cache_field_depth_choro_WorldImagery.png",
  plot = cache_field_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# ==== BOX ELDER COMPANIES DEPTH CHORO ==================================================

# Filter county spatial data to Box Elder County
boxelder = counties |> filter(county == "Box Elder")

# Fetch satellite imagery tile for Box Elder County basemap
boxelder_tile = get_tiles(
  boxelder,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE,
  project = TRUE
)

# Esri.WorldImagery zoom = 11
# Esri.WorldTopoMap zoom = 10
# Esri.WorldGrayCanvas zoom = 10

# Get spatial extent of tile
boxelder_tile_extent = ext(boxelder_tile)

# Convert tile to polygon then sf object
boxelder_tile_poly = as.polygons(boxelder_tile_extent) |> st_as_sf() |> st_set_crs(st_crs(boxelder_tile))

# Transform Box Elder County to align with tile's CRS, convert to terra vector
boxelder_vect = st_transform(boxelder, crs(boxelder_tile)) |> vect()

# Crop and mask satellite imagery tile to Box Elder County
boxelder_tile_crop = crop(boxelder_tile, boxelder_vect)
boxelder_tile_mask = mask(boxelder_tile_crop, boxelder_vect)

# Choropleth of field-level median annual depletion depth in Box Elder County
boxelder_field_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = boxelder_tile_mask) +
  # Color code each Box Elder County field by median annual depletion depth
  geom_sf(
    data = field_depletion |> filter(county == "Box Elder"), 
    aes(fill = depletion_ft), 
    color = NA
  ) + 
  # # Add Box Elder County boundary
  # geom_sf(
  #   data = boxelder,
  #   fill = NA,
  #   color = "black",
  #   linewidth = 0.3
  # ) +
  # Add outlined labels of cities and towns
  # geom_shadowtext(
  #   data = cities |> st_filter(boxelder),
  #   aes(x = X, y = Y, label = city),
  #   color = "black",
  #   bg.color = "white",
  #   bg.r = 0.1,
  #   size = 3,
  #   fontface = "bold",
  #   inherit.aes = FALSE
  # ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE),
      #0.8, 1.6, 2.4,
      max((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "3.23"
    ),
    limits = c(
      min((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE),
      max((field_depletion |> filter(county == "Box Elder"))$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "Field-Level Median Annual Depletion Depth, Box Elder County", fill = "Depletion (AFA)") +
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
    legend.title = element_text(size = 14, margin = margin(b = 10)), # Adjust legend title
    legend.text = element_text(size = 12), # Adjust legend tick labels
    # plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
    plot.title = element_blank()
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = "black",
      frame.linewidth = 0.2,
      ticks.colour = NA
    )
  )
boxelder_field_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/boxelder_field_depth_choro_WorldImagery.png",
  plot = boxelder_field_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)


check1 = box_elder_fields_company |> 
  st_drop_geometry() |> 
  group_by(irr_method) |>
  count()

check2 = box_elder_fields_independent |> 
  st_drop_geometry() |> 
  group_by(irr_method) |>
  count()

