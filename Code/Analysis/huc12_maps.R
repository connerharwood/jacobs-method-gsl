
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

# Load Utah HUC12s
huc12s_all = st_read("Data/Raw/Watersheds/HUC12/Utah_HUCs.shp") |> 
  select(huc12 = HUC_12, huc12_name = HU_12_NAME) |> 
  st_make_valid() |> 
  st_transform(crs = 26912) |> 
  mutate(huc12_acres = as.numeric(st_area(geometry)) / 4046.8564224)

huc12s_basin = huc12s_all |> 
  st_intersection(basin_boundary)

huc12s = huc12s_all |> 
  st_intersection(basin_boundary) |> 
  mutate(
    overlap_acres = as.numeric(st_area(geometry)) / 4046.8564224,
    overlap_percent = overlap_acres / huc12_acres
  ) |> 
  filter(overlap_percent > 0.05)

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

fields_huc12 = fields |> 
  st_intersection(huc12s) |> 
  mutate(
    # Calculate area of field and POU intersection
    area_overlap = as.numeric(st_area(geom)) / 4046.8564224,
    # Calculate proportion of overlap between each field and POU polygon
    percent_overlap = area_overlap / acres
  ) |> 
  # Evaluate intersections for each field and year
  group_by(id) |> 
  # For each field and year, keep only the feature with highest overlap with POU
  slice_max(order_by = percent_overlap, n = 1, with_ties = FALSE)

huc12_depletion = depletion_annual |> 
  left_join(
    fields_huc12 |> 
      st_drop_geometry() |> 
      select(id, huc12, huc12_name),
    by = "id",
    relationship = "many-to-one"
  ) |> 
  group_by(year, huc12, huc12_name) |> 
  mutate(depletion_af = sum(depletion_af, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(huc12, huc12_name) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  right_join(
    huc12s,
    by = c("huc12", "huc12_name"),
    relationship = "one-to-many"
  ) |> 
  st_as_sf()
# ==== HUC12 DEPTH CHORO =======================================================

huc12_depletion_all = huc12s_basin |> 
  left_join(
    huc12_depletion |> 
      st_drop_geometry() |> 
      select(huc12, depletion_ft, depletion_af),
    by = "huc12",
    relationship = "one-to-one"
  )

# Manually designate where to label county names on map
county_labels_huc12 = counties |> 
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

# Choropleth of field-level median annual depletion depth in GSL Basin
huc12_depth_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = huc12_depletion_all, 
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
  # Add GSL
  geom_sf(
    data = huc12s |> filter(huc12_name == "Great Salt Lake"),
    color = "black",
    fill = "white",
    linewidth = 0.3
  ) +
  # Create outlined label for GSL
  geom_shadowtext(
    data = huc12s |> 
      filter(huc12_name == "Great Salt Lake") |> 
      st_centroid() |> 
      st_coordinates() |> 
      as.data.frame() |> 
      mutate(basin = "Great Salt Lake"),
    aes(x = X, y = Y, label = basin),
    color = "black",
    bg.color = "white",
    bg.r = 0.1,
    size = 4.5,
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = 3000,
    nudge_y = -8000
  ) +
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels_huc12,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.09,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels_huc12$nudge_x,
    nudge_y = county_labels_huc12$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion depth
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(huc12_depletion_all$depletion_ft, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(huc12_depletion_all$depletion_ft, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "2.31"
    ),
    limits = c(
      min(huc12_depletion_all$depletion_ft, na.rm = TRUE),
      max(huc12_depletion_all$depletion_ft, na.rm = TRUE)
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
huc12_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/huc12_depth_choro.png",
  plot = huc12_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)

# ==== HUC12 VOLUME CHORO ======================================================

# Choropleth of field-level median annual depletion volume in GSL Basin
huc12_volume_choro = ggplot() +
  # Color code each field by median annual depletion volume
  geom_sf(
    data = huc12_depletion_all, 
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
  # Add GSL
  geom_sf(
    data = huc12s |> filter(huc12_name == "Great Salt Lake"),
    color = "black",
    fill = "white",
    linewidth = 0.3
  ) +
  # Create outlined label for GSL
  geom_shadowtext(
    data = huc12s |> 
      filter(huc12_name == "Great Salt Lake") |> 
      st_centroid() |> 
      st_coordinates() |> 
      as.data.frame() |> 
      mutate(basin = "Great Salt Lake"),
    aes(x = X, y = Y, label = basin),
    color = "black",
    bg.color = "white",
    bg.r = 0.1,
    size = 4.5,
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = 3000,
    nudge_y = -8000
  ) +
  # Create outlined labels for each county
  geom_shadowtext(
    data = county_labels_huc12,
    aes(x = X, y = Y, label = county),
    color = "black",
    bg.color = "white",
    bg.r = 0.12,
    size = 4.5, 
    fontface = "bold",
    family = "Lato",
    inherit.aes = FALSE,
    nudge_x = county_labels_huc12$nudge_x,
    nudge_y = county_labels_huc12$nudge_y
  ) +
  # Create continuous, sequential color scale for depletion volume
  scale_fill_gradientn(
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4",
               "#7fcdbb", "#41b6c4", "#1d91c0",
               "#225ea8", "#253494", "#081d58"),
    na.value = "darkgray",
    breaks = c(
      min(huc12_depletion_all$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(huc12_depletion_all$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "44,328"
    ),
    limits = c(
      min(huc12_depletion_all$depletion_af, na.rm = TRUE),
      max(huc12_depletion_all$depletion_af, na.rm = TRUE)
    )
  ) +
  # Add plot and legend titles
  labs(title = "HUC12-Level Median Annual Depletion Volume, GSL Basin", fill = "Depletion (AF)") +
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
    #plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
    plot.title = element_blank(),
    legend.position = c(0.88, 0.9),
    legend.justification = c(0, 1),
    text = element_text(color = "black", family = "Lato"),
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
huc12_volume_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/huc12_volume_choro.png",
  plot = huc12_volume_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 500
)
