
library(tidyverse)
library(sf)
library(geosphere)
library(writexl)
library(tmap)
tmap_mode("view")

# ==== LOAD ====================================================================

# Load WRLU fields panel
fields = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  # Filter to 2024 fields
  filter(year == 2024)

# Load GSL subbasins
gsl_basin = st_read("Data/Raw/GSL Basin/GSLSubbasins.shp") |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912)

# Load Place of Use polygons
pou = st_read("Data/Clean/Water Rights/pou.gpkg") |> 
  # Keep only fields that intersect a POU polygon
  st_filter(fields, .predicate = st_intersects) |> 
  # Make geometry valid
  st_make_valid() |> 
  # Remove POUs that don't touch GSL Basin
  st_filter(gsl_basin, .predicate = st_intersects)

# Load field-level annual depletion data
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# Load raw points of diversion
pod_dwr = st_read("Data/Raw/Water Rights/Points of Diversion/Utah_Points_of_Diversion.gpkg") |> 
  # Select and rename needed variables
  select(
    wr_num = WRNUM,
    chex_num = CHEXNUM,
    type = TYPE,
    summary_status = SUMMARY_ST,
    status = STATUS,
    priority = PRIORITY,
    uses = USES,
    decreed_cfs = CFS,
    decreed_af = ACFT,
    owner = OWNER,
    source = SOURCE,
    wr_link = WebLink,
    geom_dwr = SHAPE
  ) |> 
  # Validate geometry
  st_make_valid() |> 
  # Transform to NAD 83 for spatial operations
  st_transform(crs = 26912) |> 
  # For character vars: remove extra whitespace, convert uppercase
  mutate(
    across(where(is.character), ~str_squish(.x)),
    across(where(is.character), ~str_to_upper(.x))
  ) |> 
  filter(
    # wr_num %in% pou$wr_num
    # Only include irrigation rights
    grepl("I", uses, ignore.case = TRUE),
    # Remove terminated rights
    summary_status != "T",
    # Remove rights whose status indicates inactivity
    !grepl("ABAN|DIS|EXP|FORF|INV|LAP|REJ|TERM|WD", status, ignore.case = TRUE),
    # Remove abandoned well rights
    type != "ABANDONDED WELL"
  ) |> 
  # Keep only rights in GSL Basin
  st_filter(gsl_basin, .predicate = st_within) |> 
  # Transform to WGS 84 to calculate distances between points
  st_transform(crs = 4326) |> 
  mutate(
    # Assign unique ID to each POD
    id_dwr = row_number(),
    
    # Grab longitude and latitude coords
    lon = st_coordinates(geom_dwr)[, 1],
    lat = st_coordinates(geom_dwr)[, 2]
  )

# Load PODs from KML files
pod_kml = st_read("Data/Clean/Water Rights/pod.gpkg") |> 
  rename(geom_kml = geom) |> 
  filter(wr_num %in% pou$wr_num) |> 
  # Keep only PODs within GSL Basin
  st_filter(gsl_basin, .predicate = st_within) |> 
  st_transform(crs = 4326) |> 
  mutate(
    id_kml = row_number(),
    lon = st_coordinates(geom_kml)[, 1],
    lat = st_coordinates(geom_kml)[, 2]
  )

# ==== INVESTIGATE POU =========================================================

kml_missing = pod_dwr |> filter(!wr_num %in% pou$wr_num)
kml_present = pod_dwr |> filter(wr_num %in% pou$wr_num)

missing_type = kml_missing |> group_by(type) |> count()
present_type = kml_present |> group_by(type) |> count()

pou_intersects = st_intersects(pou)

pou_intersects2 = pou |>
  dplyr::filter(
    lengths(
      Map(\(x, i) setdiff(x, i), pou_intersects, seq_along(pou_intersects))
    ) > 0
  )

pou_overlaps = st_overlaps(pou)

pou_overlaps2 = pou |>
  dplyr::filter(
    lengths(
      Map(\(x, i) setdiff(x, i), pou_overlaps, seq_along(pou_overlaps))
    ) > 0
  )


overlap_filter = function(x, threshold = 0.10) {
  g = st_geometry(x)
  a_self = st_area(g)
  
  nbrs = st_intersects(x)  # candidates (fast)
  
  frac = vapply(seq_along(nbrs), function(i) {
    others = setdiff(nbrs[[i]], i)
    if (length(others) == 0) return(0)
    
    # union neighbors then intersect once (avoids double counting)
    u = st_union(g[others])
    a_overlap = st_area(st_intersection(g[i], u))
    
    as.numeric(a_overlap / a_self[i])
  }, numeric(1))
  
  x |>
    mutate(overlap_frac = frac) |>
    filter(overlap_frac >= threshold)
}

pou_overlap_filtered = overlap_filter(pou, threshold = 0.90)

# ==== INVESTIGATE FIELD-POU OVERLAPS ==========================================

# Assign each field to water right it has the most overlap with
fields_pou_overlap = st_intersection(
  fields,
  pou
) |> 
  mutate(
    # Calculate area of field and POU intersection
    overlap_area = as.numeric(st_area(geom)) / 4046.8564224,
    # Calculate proportion of overlap between each field and POU polygon
    overlap_percent = overlap_area / acres
  )

check2 = fields_pou_overlap |> filter(overlap_percent >= 0.9)

total_fields = n_distinct(fields_pou_overlap$id)

fields_pou_check1 = fields_pou_overlap |> 
  #st_drop_geometry() |> 
  filter(overlap_percent >= 0.9) |> 
  group_by(id) |> 
  filter(n_distinct(wr_num) > 1) |> 
  ungroup()

fields_multiple_wrs = n_distinct(fields_pou_check1$id)
percent_fields_multiple_wrs = fields_multiple_wrs / total_fields
percent_fields_multiple_wrs

# The vast majority of fields overlap with multiple WRs' POUs
# This is the case even after only including fields with significant overlap

fields_plot1 = fields_pou_check1 |> 
  filter(id %in% 3:10)

pou_plot1 = pou |> 
  filter(wr_num %in% fields_plot1$wr_num)
  
# Inspect field identification within service area
tm_shape(fields_plot1) +
  tm_polygons(fill = "lightgreen", col = "darkgreen", group = "Fields") +
  tm_shape(pou_plot1) +
  tm_borders(col = "red", lwd = 2, lty = "dashed", group = "POU") +
  tm_layout(legend.outside = TRUE)

# Plot histogram of overlap percentages
overlap_plot = ggplot(fields_pou_overlap, aes(x = overlap_percent)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Overlap Percent",
    y = "Count of Fields",
    title = "Field-Place of Use Overlap Percent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14, angle = 50, hjust = 1),
    axis.text.y = element_text(color = "black", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(color = "black", size = 16),
    axis.line = element_line(color = "black"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16),
  )
overlap_plot

# ==== POU-FIELD LINK ==========================================================

# Assign each field to water right it has the most overlap with
overlapping_fields = st_intersection(
  fields,
  pou
) |> 
  mutate(
    # Calculate area of field and POU intersection
    overlap_area = as.numeric(st_area(geom)) / 4046.8564224,
    # Calculate proportion of overlap between each field and POU polygon
    overlap_percent = overlap_area / acres
  ) |> 
  # Evaluate intersections for each field and year
  group_by(id) |> 
  # For each field and year, keep only the feature with highest overlap with POU
  slice_max(order_by = overlap_percent, n = 1, with_ties = FALSE) |> 
  # Ungroup data
  ungroup() #|> 
  # Filter out fields with less X% overlap with service area
  # filter(overlap_percent >= 0.75)

# TONS of water rights with field overlap if I let fields be assigned to
# multiple water rights: in that case, maybe don't do slice max, but
# filter on overlap percent and do an area-weighted mean (median?)

# Must be LOTs of overlap of POUs

intersecting_pou = st_intersection(pou)

# Plot histogram of overlap percentages
overlap_plot = ggplot(overlapping_fields, aes(x = overlap_percent)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Overlap Percent",
    y = "Count of Fields",
    title = "Field-Place of Use Overlap Percent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14, angle = 50, hjust = 1),
    axis.text.y = element_text(color = "black", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(color = "black", size = 16),
    axis.line = element_line(color = "black"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16),
  )
overlap_plot

# Save plot
# ggsave(
#   "Figures/Plots/field_pou_overlap_hist.png", 
#   plot = overlap_plot,
#   dpi = 300,
#   width = 13,
#   height = 7,
#   bg = "white"
# )

# Calculate number of water rights with overlapping fields at each threshold
threshold_summary = tibble(threshold = seq(0, 1, by = 0.01)) |>
  mutate(
    n_wr_num = purrr::map_int(
      threshold,
      ~ overlapping_fields |>
        filter(overlap_percent >= .x) |>
        summarise(n = n_distinct(wr_num)) |>
        pull(n)
    )
  )

# Plot number of water rights included at each percent threshold
threshold_plot = ggplot(threshold_summary, aes(x = threshold, y = n_wr_num)) +
  geom_point(color = "black") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.02)) +
  labs(
    x = "Percent Threshold",
    y = "Water Rights with Fields",
    title = "Field-Place of Use Overlap Threshold"
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = max(threshold_summary$n_wr_num), by = 200),
    limits = c(0, max(threshold_summary$n_wr_num))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14, angle = 50, hjust = 1),
    axis.text.y = element_text(color = "black", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(color = "black", size = 16),
    axis.line = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16)
  )
threshold_plot

# Save plot
# ggsave(
#   "Figures/Plots/field_pou_threshold_plot.png", 
#   plot = threshold_plot,
#   dpi = 300,
#   width = 13,
#   height = 7,
#   bg = "white"
# )

# Only include fields with at least 5% overlap with a POU
overlapping_fields_filtered = overlapping_fields |> filter(overlap_percent >= 0.05)

# Identify qualified fields
fields_wr = fields |>
  left_join(
    # Join all fields with qualified fields
    overlapping_fields_filtered |> st_drop_geometry() |> select(id, wr_num, overlap_percent), 
    by = c("id")
  ) |> 
  # Filter out fields that don't have at least 5% overlap with a water right POU
  filter(!is.na(wr_num))

# Inspect field identification within service area
tm_shape(fields_wr) +
  tm_polygons(fill = "lightgreen", col = "darkgreen", group = "Fields") +
  tm_shape(pou |> filter(wr_num %in% fields_wr$wr_num)) +
  tm_borders(col = "red", lwd = 2, lty = "dashed", group = "POU") +
  tm_layout(legend.outside = TRUE)

# PLOT EVERYTHING
tm_shape(fields) +
  tm_polygons(fill = "lightgreen", col = "darkgreen", group = "Fields") +
  tm_shape(pou) +
  tm_borders(col = "red", lwd = 2, lty = "dashed", group = "POU") +
  tm_layout(legend.outside = TRUE)

# ==== COLLAPSE POD ============================================================

pod_kml_dwr = pod_kml |> 
  as.data.frame() |> 
  left_join(
    pod_dwr |> as.data.frame(),
    by = "wr_num",
    relationship = "many-to-many"
  ) |> 
  filter(wr_num %in% fields_wr$wr_num)

pod_dist = pod_kml_dwr |> 
  rowwise() |> 
  mutate(dist_meters = distm(
    x = c(lon.x, lat.x),
    y = c(lon.y, lat.y),
    fun = distGeo
  )[1,1]) |> 
  filter(dist_meters < 1.2119)

# Determine which sensible distance threshold retains the most water rights
dist_threshold = pod_dwr |> filter(id_dwr %in% pod_dist$id_dwr)
n_distinct(dist_threshold$wr_num)

ggplot(pod_dist, aes(x = dist_meters)) +
  geom_histogram(bins = 200) +
  #scale_x_continuous(breaks = seq(from = 0, to = max(pod_dist$dist_meters), by = 5000)) +
  theme(axis.text.x = element_text(angle = 50))

pod_unique = pod_dwr |> 
  st_drop_geometry() |> 
  filter(id_dwr %in% pod_dist$id_dwr) |> 
  mutate(
    across(where(is.character), ~na_if(.x, "")),
    across(where(is.character), ~str_squish(.x)),
    priority = as.character(priority),
    priority = case_when(
      nchar(priority) == 4 ~ paste0(priority, "-01-01"),
      nchar(priority) == 6 ~ paste0(substr(priority, 1, 4), "-",
                                    substr(priority, 5, 6), "-01"),
      nchar(priority) == 8 ~ paste0(substr(priority, 1, 4), "-",
                                    substr(priority, 5, 6), "-",
                                    substr(priority, 7, 8)),
      TRUE ~ NA_character_
    ),
    decreed_cfs = ifelse(decreed_cfs == 0, NA, decreed_cfs),
    decreed_af = ifelse(decreed_af == 0, NA, decreed_af),
    priority = as.Date(priority)
  ) |> 
  group_by(wr_num) |> 
  summarize(
    across(
      everything(),
      ~paste(unique(na.omit(.x)), collapse = "; "),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |> 
  mutate(across(where(is.character), ~na_if(.x, "")))

# ==== ANNUAL DEPLETION ========================================================

fields_wr_depletion_annual = fields_wr |> 
  st_drop_geometry() |> 
  select(-year) |> 
  left_join(
    depletion_annual |> select(id, year, depletion_ft, depletion_af),
    by = "id",
    relationship = "one-to-many"
  ) |> 
  mutate(depletion_af_weighted = depletion_af * overlap_percent)
  
wr_depletion_annual = fields_wr_depletion_annual |> 
  group_by(wr_num, year) |> 
  mutate(depletion_af_total = sum(depletion_af_weighted, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(wr_num) |> 
  summarize(
    depletion_ft = median(depletion_ft, na.rm = TRUE),
    depletion_af = median(depletion_af_total, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(
    pod_unique, by = "wr_num"
  ) |> 
  select(
    `Water Right Number` = wr_num,
    `Owner` = owner,
    `Priority Date` = priority,
    Type = type,
    Source = source,
    Uses = uses,
    `Decreed CFS` = decreed_cfs,
    `Decreed AF` = decreed_af,
    Link = wr_link,
    `Median Annual Depletion (AFA)` = depletion_ft,
    `Median Annual Depletion (AF)` = depletion_af
  ) |> 
  arrange(desc(`Median Annual Depletion (AFA)`))

# ==== SAVE ====================================================================

write_xlsx(wr_depletion_annual, "Data/Clean/Depletion/Utah/wr_depletion_annual.xlsx")

# ==== FIELDS ANALYSIS =========================================================

fields_present = fields_wr
fields_missing = fields |> filter(!id %in% fields_present$id)

present_check1 = fields_present |>
  st_drop_geometry() |>
  count(irr_method, name = "n") |>
  mutate(prop_total = n / sum(n)) |> 
  arrange(desc(prop_total))

missing_check1 = fields_missing |>
  st_drop_geometry() |>
  count(irr_method, name = "n") |>
  mutate(prop_total = n / sum(n)) |> 
  arrange(desc(prop_total))

present_check2 = fields_present |>
  st_drop_geometry() |>
  count(land_use_group, name = "n") |>
  mutate(prop_total = n / sum(n)) |> 
  arrange(desc(prop_total))

missing_check2 = fields_missing |>
  st_drop_geometry() |>
  count(land_use_group, name = "n") |>
  mutate(prop_total = n / sum(n)) |> 
  arrange(desc(prop_total))

check = wr_depletion_annual |>
  st_drop_geometry() |>
  count(Type, name = "n") |>
  mutate(prop_total = n / sum(n)) |> 
  arrange(desc(prop_total))

