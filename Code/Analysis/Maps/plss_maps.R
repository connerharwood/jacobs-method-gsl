
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

quarter_quarters = st_read("Data/Raw/PLSS/plss_layers.gpkg", layer = "quarter_quarter") |> 
  select(quarter_quarter_id = OBJECTID) |> 
  st_make_valid() |> 
  st_intersection(basin_boundary)

quarters = st_read("Data/Raw/PLSS/plss_layers.gpkg", layer = "quarter") |> 
  select(quarter_id = OBJECTID) |> 
  st_make_valid() |> 
  st_intersection(basin_boundary)

sections = st_read("Data/Raw/PLSS/plss_layers.gpkg", layer = "section") |> 
  select(section_id = OBJECTID) |> 
  st_make_valid() |> 
  st_intersection(basin_boundary)

townships = st_read("Data/Raw/PLSS/plss_layers.gpkg", layer = "township") |> 
  select(township_id = OBJECTID) |> 
  st_make_valid() |> 
  st_intersection(basin_boundary)

# ==== PREP ====================================================================

fields_quarter_quarters = fields |> 
  st_intersection(quarter_quarters) |> 
  # Calculate area of field and POU intersection
  mutate(
    overlap_acres = as.numeric(st_area(geom)) / 4046.8564224,
    overlap_share = overlap_acres / acres
  ) |> 
  st_drop_geometry() |> 
  group_by(id) |>
  mutate(overlap_share = overlap_share / sum(overlap_share, na.rm = TRUE)) |>
  ungroup()

fields_quarters = fields |> 
  st_intersection(quarters) |> 
  # Calculate area of field and POU intersection
  mutate(
    overlap_acres = as.numeric(st_area(geom)) / 4046.8564224,
    overlap_share = overlap_acres / acres
  ) |> 
  st_drop_geometry() |> 
  group_by(id) |>
  mutate(overlap_share = overlap_share / sum(overlap_share, na.rm = TRUE)) |>
  ungroup()

fields_sections = fields |> 
  st_intersection(sections) |> 
  # Calculate area of field and POU intersection
  mutate(
    overlap_acres = as.numeric(st_area(geom)) / 4046.8564224,
    overlap_share = overlap_acres / acres
  ) |> 
  st_drop_geometry() |> 
  group_by(id) |>
  mutate(overlap_share = overlap_share / sum(overlap_share, na.rm = TRUE)) |>
  ungroup()

fields_townships = fields |> 
  st_intersection(townships) |> 
  # Calculate area of field and POU intersection
  mutate(
    overlap_acres = as.numeric(st_area(geom)) / 4046.8564224,
    overlap_share = overlap_acres / acres
  ) |> 
  st_drop_geometry() |> 
  group_by(id) |>
  mutate(overlap_share = overlap_share / sum(overlap_share, na.rm = TRUE)) |>
  ungroup()

quarter_quarters_depletion = depletion_annual |> 
  left_join(
    fields_quarter_quarters |> select(id, quarter_quarter_id, overlap_acres, overlap_share),
    by = "id",
    relationship = "many-to-many"
  ) |> 
  mutate(depletion_af_quarter_quarter = depletion_af * overlap_share) |> 
  group_by(quarter_quarter_id, year) |> 
  summarize(
    depletion_ft = weighted.mean(depletion_ft, w = overlap_acres, na.rm = TRUE),
    depletion_af = sum(depletion_af_quarter_quarter, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(quarter_quarter_id) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  right_join(
    quarter_quarters,
    by = "quarter_quarter_id",
    relationship = "one-to-one"
  ) |> 
  st_as_sf()

quarters_depletion = depletion_annual |> 
  left_join(
    fields_quarters |> select(id, quarter_id, overlap_acres, overlap_share),
    by = "id",
    relationship = "many-to-many"
  ) |> 
  mutate(depletion_af_quarter = depletion_af * overlap_share) |> 
  group_by(quarter_id, year) |> 
  summarize(
    depletion_ft = weighted.mean(depletion_ft, w = overlap_acres, na.rm = TRUE),
    depletion_af = sum(depletion_af_quarter, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(quarter_id) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  right_join(
    quarters,
    by = "quarter_id",
    relationship = "one-to-one"
  ) |> 
  st_as_sf()

sections_depletion = depletion_annual |> 
  left_join(
    fields_sections |> select(id, section_id, overlap_acres, overlap_share),
    by = "id",
    relationship = "many-to-many"
  ) |> 
  mutate(depletion_af_section = depletion_af * overlap_share) |> 
  group_by(section_id, year) |> 
  summarize(
    depletion_ft = weighted.mean(depletion_ft, w = overlap_acres, na.rm = TRUE),
    depletion_af = sum(depletion_af_section, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(section_id) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  right_join(
    sections,
    by = "section_id",
    relationship = "one-to-one"
  ) |> 
  st_as_sf()

townships_depletion = depletion_annual |> 
  left_join(
    fields_townships |> select(id, township_id, overlap_acres, overlap_share),
    by = "id",
    relationship = "many-to-many"
  ) |> 
  mutate(depletion_af_township = depletion_af * overlap_share) |> 
  group_by(township_id, year) |> 
  summarize(
    depletion_ft = weighted.mean(depletion_ft, w = overlap_acres, na.rm = TRUE),
    depletion_af = sum(depletion_af_township, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(township_id) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  right_join(
    townships,
    by = "township_id",
    relationship = "one-to-one"
  ) |> 
  st_as_sf()

# Manually designate where to label county names on map
county_labels = counties |> 
  st_point_on_surface() |> 
  # Grab X and Y coordinates
  st_coordinates() |>
  # Convert to df
  as.data.frame() |> 
  # Join with county name
  cbind(county = counties$county) |> 
  # Remove county labels for certain counties
  filter(!county %in% c("Sanpete", "Carbon", "Duchesne")) |> 
  # Manually nudge location of each county label
  mutate(
    nudge_x = case_when(
      county == "Box Elder" ~ 30000, # Move right
      county == "Davis" ~ 9500, # Move right
      county == "Summit" ~ -32000, # Move left
      county == "Tooele" ~ 50000, # Move right
      county == "Juab" ~ 90000, # Move right
      county == "Wasatch" ~ -17000, # Move left
      county == "Morgan" ~ 5000, # Move right
      county == "Cache" ~ 2000, # Move right
      county == "Weber" ~ 5000, # Move right
      county == "Rich" ~ 1000, # Move right
      #county == "Duchesne" ~ -18000, # Move left
      TRUE ~ 0
    ),
    nudge_y = case_when(
      county == "Box Elder" ~ 32000, # Move up
      county == "Davis" ~ 12000, # Move up
      county == "Summit" ~ -5000, # Move down
      county == "Morgan" ~ 6000, # Move up
      county == "Weber" ~ 3000, # Move up
      #county == "Duchesne" ~ -20000, # Move down
      county == "Wasatch" ~ 21000, # Move up
      county == "Juab" ~ 9000, # Move up
      county == "Cache" ~ 2000, # Move up
      county == "Rich" ~ 4000, # Move up
      TRUE ~ 0
    )
  )

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

# ==== QUARTER-QUARTERS ========================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
qq_depth_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = quarter_quarters_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(quarter_quarters_depletion$depletion_ft, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(quarter_quarters_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "3.16"
    ),
    limits = c(
      min(quarter_quarters_depletion$depletion_ft, na.rm = TRUE),
      max(quarter_quarters_depletion$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
qq_depth_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
qq_volume_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = quarter_quarters_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(quarter_quarters_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(quarter_quarters_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "249.26"
    ),
    limits = c(
      min(quarter_quarters_depletion$depletion_af, na.rm = TRUE),
      max(quarter_quarters_depletion$depletion_af, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
qq_volume_choro

# ==== QUARTERS ================================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
q_depth_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = quarters_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(quarters_depletion$depletion_ft, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(quarters_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "3.16"
    ),
    limits = c(
      min(quarters_depletion$depletion_ft, na.rm = TRUE),
      max(quarters_depletion$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
q_depth_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
q_volume_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = quarters_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(quarters_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(quarters_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "470.34"
    ),
    limits = c(
      min(quarters_depletion$depletion_af, na.rm = TRUE),
      max(quarters_depletion$depletion_af, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
q_volume_choro

# ==== SECTIONS ================================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
s_depth_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = sections_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(sections_depletion$depletion_ft, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(sections_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "2.82"
    ),
    limits = c(
      min(sections_depletion$depletion_ft, na.rm = TRUE),
      max(sections_depletion$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
s_depth_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
s_volume_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = sections_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(sections_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(sections_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "1673.21"
    ),
    limits = c(
      min(sections_depletion$depletion_af, na.rm = TRUE),
      max(sections_depletion$depletion_af, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
s_volume_choro

# ==== TOWNSHIPS ===============================================================

# Choropleth of field-level median annual depletion depth in GSL Basin
t_depth_choro = ggplot() +
  # Add satellite imagery basemap
  layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = townships_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(townships_depletion$depletion_ft, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(townships_depletion$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "2.47"
    ),
    limits = c(
      min(townships_depletion$depletion_ft, na.rm = TRUE),
      max(townships_depletion$depletion_ft, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
t_depth_choro

# Choropleth of field-level median annual depletion volume in GSL Basin
t_volume_choro = ggplot() +
  # # Add satellite imagery basemap
  # layer_spatial(data = basin_tile_mask) +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = townships_depletion, 
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
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels$nudge_x,
    nudge_y = county_labels$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(townships_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(townships_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "30868"
    ),
    limits = c(
      min(townships_depletion$depletion_af, na.rm = TRUE),
      max(townships_depletion$depletion_af, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Depth, GSL Basin", fill = "Depletion (AFA)") +
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
    legend.position = c(0.88, 0.9), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    text = element_text(color = "black", family = "Lato")
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
t_volume_choro
