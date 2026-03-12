
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

# Load Utah HUC12s
huc12s_all = st_read("Data/Raw/Watersheds/HUC12/Utah_HUCs.shp") |> 
  select(huc12 = HUC_12, huc12_name = HU_12_NAME) |> 
  st_make_valid() |> 
  st_transform(crs = 26912) |> 
  mutate(huc12_acres = as.numeric(st_area(geometry)) / 4046.8564224)

huc12s = huc12s_all |> 
  st_intersection(basin_boundary) |> 
  mutate(
    overlap_acres = as.numeric(st_area(geometry)) / 4046.8564224,
    overlap_percent = overlap_acres / huc12_acres
  ) |> 
  filter(overlap_percent > 0.05)

# ==== PREP ====================================================================

# Calculate average annual depletion depth and volume for each county
county_depletion = depletion_annual |> 
  # Group data by county and year
  group_by(county, year) |> 
  # Calculate total depletion volume for each county and year
  mutate(depletion_af_total = sum(depletion_af, na.rm = TRUE)) |> 
  # Ungroup data
  ungroup() |> 
  # Group data by county
  group_by(county) |> 
  # Calculate median depletion depth and median total depletion volume for each county
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Join with county spatial data
  right_join(
    counties,
    by = "county",
    relationship = "one-to-one"
  ) |> 
  # Convert df to sf
  st_as_sf()

# ==== COUNTY DEPTH CHORO ======================================================

# Create new GSL Basin boundary but without lines along Utah border
basin_boundary2 = gsl_basin |> 
  st_union() |> 
  st_boundary() |> 
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
  # Manually nudge location of each county label
  mutate(
    nudge_x = case_when(
      county == "Box Elder" ~ 30000, # Move right
      county == "Davis" ~ 9000, # Move right
      county == "Summit" ~ -30000, # Move left
      county == "Tooele" ~ 50000, # Move right
      TRUE ~ 0
    ),
    nudge_y = case_when(
      county == "Box Elder" ~ 32000, # Move up
      county == "Davis" ~ 13000, # Move up
      county == "Juab" ~ 7000, # Move up
      county == "Summit" ~ -5000, # Move down
      county == "Morgan" ~ 5000, # Move up
      county == "Weber" ~ 2000, # Move up
      TRUE ~ 0
    )
  )

# Choropleth of median annual depletion depth by GSL Basin county 
county_depth_choro = ggplot() + 
  # Color code each county by median annual depletion depth 
  geom_sf(data = county_depletion, aes(fill = depletion_ft), color = NA) + 
  # Add county boundaries 
  geom_sf(data = county_depletion, color = "black", fill = NA, linewidth = 0.2) + 
  # Add GSL 
  geom_sf( 
    data = huc12s |> filter(huc12_name == "Great Salt Lake"), 
    color = "black", 
    fill = "white", 
    linewidth = 0.2 
  ) + 
  # Add GSL Basin boundary 
  geom_sf( 
    data = basin_boundary2 |> st_intersection(counties), 
    aes(color = "GSL Basin"), 
    fill = NA, 
    linewidth = 0.4 ,
    show.legend = FALSE
  ) + 
  # # Add GSL Basin boundary color to legend 
  # scale_color_manual( 
  #   name = "", 
  #   values = c("GSL Basin" = "red") 
  # ) + 
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
    size = 3, 
    fontface = "bold", 
    inherit.aes = FALSE, 
    nudge_x = 3000, 
    nudge_y = -8000 
  ) + 
  # Create outlined labels for each county 
  geom_shadowtext( 
    data = county_labels, 
    aes(x = X, y = Y, label = county), 
    color = "black", 
    bg.color = "white", 
    bg.r = 0.12, 
    size = 3, 
    fontface = "bold", 
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
      min(county_depletion$depletion_ft, na.rm = TRUE), 
      # 0.8, 1.6, 2.4, 
      max(county_depletion$depletion_ft, na.rm = TRUE) 
    ), 
    labels = expression( 
      "0.57", 
      # "0.8", # "1.6", # "2.4", 
      "1.85" 
    ), 
    limits = c( 
      min(county_depletion$depletion_ft, na.rm = TRUE), 
      max(county_depletion$depletion_ft, na.rm = TRUE) ) 
  ) + 
  # Add plot and legend titles 
  labs(title = "County-Level Median Annual Depletion Depth", fill = "Depletion (AFA)") + 
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
    legend.text = element_text(size = 12), # Adjust legend ticks text 
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
    legend.position = c(0.77, 0.936), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    #legend.spacing.y = unit(-0.3, "cm")
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
county_depth_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/county_depth_choro.png",
  plot = county_depth_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 400
)

# ==== COUNTY VOLUME CHORO =====================================================

# Choropleth of median annual depletion volume by GSL Basin county 
county_volume_choro = ggplot() + 
  # Color code each county by median annual depletion volume 
  geom_sf( 
    data = county_depletion, 
    aes(fill = depletion_af), 
    color = NA 
  ) + 
  # Add county boundaries 
  geom_sf( 
    data = county_depletion, 
    color = "black", 
    fill = NA, 
    linewidth = 0.2 
  ) + 
  # Add GSL 
  geom_sf( 
    data = huc12s |> filter(huc12_name == "Great Salt Lake"), 
    color = "black", 
    fill = "white", 
    linewidth = 0.2 
  ) + 
  # Add GSL Basin boundary 
  geom_sf( 
    data = basin_boundary2 |> st_intersection(counties), 
    aes(color = "GSL Basin"), 
    fill = NA, 
    linewidth = 0.4 ,
    show.legend = FALSE
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
    size = 3, 
    fontface = "bold", 
    inherit.aes = FALSE, 
    nudge_x = 3000, 
    nudge_y = -8000 
  ) + 
  # Create outlined labels for each county 
  geom_shadowtext( 
    data = county_labels, 
    aes(x = X, y = Y, label = county), 
    color = "black", 
    bg.color = "white", 
    bg.r = 0.12, 
    size = 3, 
    fontface = "bold", 
    inherit.aes = FALSE, 
    nudge_x = county_labels$nudge_x, 
    nudge_y = county_labels$nudge_y 
  ) + 
  # Create continuous, sequential color scale for depletion volume 
  scale_fill_gradientn( 
    colors = c("#ffffd9", "#edf8b1", "#c7e9b4", 
               "#7fcdbb", "#41b6c4", "#1d91c0", 
               "#225ea8", "#253494", "#081d58"), 
    na.value = "darkgray", 
    breaks = c( 
      min(county_depletion$depletion_af, na.rm = TRUE), 
      # 0.8, 1.6, 2.4, 
      max(county_depletion$depletion_af, na.rm = TRUE) 
    ), 
    labels = expression( 
      "4,549", 
      # "0.8", # "1.6", # "2.4", 
      "302,107" 
    ), 
    limits = c( 
      min(county_depletion$depletion_af, na.rm = TRUE), 
      max(county_depletion$depletion_af, na.rm = TRUE) ) 
  ) + 
  # Add plot and legend titles 
  labs(title = "County-Level Median Annual Depletion Volume", fill = "Depletion (AF)") + 
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
    legend.text = element_text(size = 12), # Adjust legend ticks text 
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust plot title
    legend.position = c(0.77, 0.936), # Adjust legend position
    legend.justification = c(0, 1), # Adjust legend position
    #legend.spacing.y = unit(-0.3, "cm")
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
county_volume_choro

# Save as high-resolution PNG image
ggsave(
  "Figures/Maps/county_volume_choro.png",
  plot = county_volume_choro, 
  width = 16, 
  height = 10, 
  units = "in", 
  dpi = 400
)
