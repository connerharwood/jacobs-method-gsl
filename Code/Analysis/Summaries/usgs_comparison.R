
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  # Select only basin name
  select(basin = Name) |> 
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
  st_filter(gsl_basin, .predicate = st_intersects)

# Intersect GSL Basin with Utah counties, dissolved into one outer boundary
basin_boundary = gsl_basin |> 
  st_intersection(counties) |> 
  st_union() |> 
  st_exterior_ring() |> 
  st_as_sf()

# Load WRLU ag fields
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  # Filter to just 2024 cross section
  filter(year == 2024) |> 
  # Filter to fields that intersect GSL Basin
  st_filter(basin_boundary, .predicate = st_intersects)

# Load field-level annual depletions
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# Load Utah HUC12s
huc12s = st_read("Data/Raw/Watersheds/HUC12/Utah_HUCs.shp") |> 
  select(huc12 = HUC_12, huc12_name = HU_12_NAME) |> 
  st_make_valid() |> 
  st_transform(crs = 26912) |> 
  st_intersection(basin_boundary)

# Load USGS HUC12-level annual irrigation consumptive use data
usgs_annual = read_csv("Data/Raw/USGS Irrigation Water Use/Irr_CU_HUC12_Tot_annual_2000_2020.csv")

# ==== PREP ====================================================================

# Pivot USGS water use data from wide to long format
usgs_annual_long = pivot_longer(
  usgs_annual,
  cols = -Year,
  names_to = "huc12",
  values_to = "depletion"
)


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

jm_depletion = depletion_annual |> 
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

usgs_depletion = usgs_annual_long |> 
  mutate(
    depletion = if_else(depletion == 999, NA, depletion),
    depletion_af = depletion * 1e6 * 365 / 325851
  ) |> 
  filter(Year >= 2017) |> 
  group_by(huc12) |> 
  summarize(depletion_af = median(depletion_af, na.rm = TRUE), .groups = "drop")

merge = left_join(
  jm_depletion |> st_drop_geometry(),
  usgs_depletion,
  by = "huc12",
  relationship = "one-to-one"
) |> 
  mutate(diff = depletion_af.x - depletion_af.y)

ggplot(merge, aes(x = diff)) +
  geom_histogram()

ggplot(merge, aes(x = depletion_af.x, y = depletion_af.y)) +
  geom_point()




jacobs_method = jm_depletion |> 
  st_drop_geometry() |> 
  select(huc12, depletion_af) |> 
  left_join(huc12s |> select(huc12), by = "huc12") |> 
  st_as_sf()

usgs_method = usgs_depletion |> 
  filter(huc12 %in% huc12s$huc12) |> 
  left_join(huc12s |> select(huc12), by = "huc12") |> 
  st_as_sf()

# Choropleth of field-level median annual depletion depth in GSL Basin
jm_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = jacobs_method, 
    aes(fill = depletion_af), 
    color = NA
  ) +
  # Add GSL
  geom_sf(
    data = huc12s |> filter(huc12_name == "Great Salt Lake"),
    color = "darkblue",
    fill = "white",
    linewidth = 0.4
  ) +
  geom_sf_text(
    data = huc12s |> filter(huc12_name == "Great Salt Lake"),
    aes(label = "Great Salt Lake"),
    color = "black",
    size = 4,
    fontface = "bold",
    nudge_y = -6000
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
      min(huc12_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(huc12_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "2.26"
    ),
    limits = c(
      min(huc12_depletion$depletion_af, na.rm = TRUE),
      max(huc12_depletion$depletion_af, na.rm = TRUE)
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
    plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = NA,
      ticks.colour = "black"
    )
  )
jm_choro

# Choropleth of field-level median annual depletion depth in GSL Basin
usgs_choro = ggplot() +
  # Color code each field by median annual depletion depth
  geom_sf(
    data = usgs_method, 
    aes(fill = depletion_af), 
    color = NA
  ) +
  # Add GSL
  geom_sf(
    data = huc12s |> filter(huc12_name == "Great Salt Lake"),
    color = "darkblue",
    fill = "white",
    linewidth = 0.4
  ) +
  geom_sf_text(
    data = huc12s |> filter(huc12_name == "Great Salt Lake"),
    aes(label = "Great Salt Lake"),
    color = "black",
    size = 4,
    fontface = "bold",
    nudge_y = -6000
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
      min(huc12_depletion$depletion_af, na.rm = TRUE),
      # 0.8, 1.6, 2.4,
      max(huc12_depletion$depletion_af, na.rm = TRUE)
    ),
    labels = expression(
      "0",
      # "0.8",
      # "1.6",
      # "2.4",
      "2.26"
    ),
    limits = c(
      min(huc12_depletion$depletion_af, na.rm = TRUE),
      max(huc12_depletion$depletion_af, na.rm = TRUE)
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
    plot.title = element_text(size = 20, hjust = 0.5) # Adjust plot title
  ) +
  # Adjust color scale bar
  guides(
    fill = guide_colorbar(
      barheight = unit(2, "in"),
      frame.colour = NA,
      ticks.colour = "black"
    )
  )
usgs_choro
