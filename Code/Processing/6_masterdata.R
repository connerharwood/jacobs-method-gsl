
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/Utah/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/Input Data/Utah/ssurgo.rda") # Soil data
load("Data/Clean/Input Data/Utah/prism.rda") # Monthly precipitation data
load("Data/Clean/Input Data/Utah/openet_eemetric.rda") # Monthly ET data

# ==== MERGE ===================================================================

# Merge all input datasets into one
merge = expand_grid(
  # Create crosswalk of all fields, years, and months
  id = unique(fields_panel$id),
  year = 2016:2024,
  month = 1:12
) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  # Only include relevant months
  filter(water_year %in% 2017:2024) |> 
  # Join with yearly crop rooting zone depth and acres
  left_join(
    fields_panel |> select(id, year, acres, rz_in),
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  ) |> 
  # Join with time-invariant SSURGO soil data
  left_join(
    ssurgo,
    by = "id",
    relationship = "many-to-one"
  ) |> 
  # Ensure that rooting depth doesn't exceed water table or bedrock
  mutate(rz_in = case_when(
    is.na(rz_in) ~ rz_in, # Keep NA as is
    !is.na(rz_in) & rz_in > max_rz_in ~ max_rz_in, # Set rz to restrictive layer if it exceeds it
    TRUE ~ rz_in # Keep all other cases as is
  )) |> 
  # Join with monthly precipitation data
  left_join(
    prism,
    by = c("id", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  # Join with monthly ET data
  left_join(
    openet_eemetric,
    by = c("id", "year", "month"),
    relationship = "one-to-one"
  )

# ==== DEPLETION INPUTS ========================================================

depletion_data1 = merge |> 
  # Calculate monthly effective precipitation
  mutate(peff_in = pmax(0, swsf * (0.70917 * prcp_in ^ 0.82416 - 0.11556) * 10 ^ (0.02426 * et_in))) |> 
  group_by(id, water_year) |> 
  # Calculate winter carryover soil moisture
  mutate(smco_in = pmax(0, pmin(0.67 * (prcp_win_in - 1.25 * et_win_in), 0.75 * rz_in * awc_in_in))) |> 
  ungroup()

depletion_data2 = depletion_data1 |> 
  # Filter to April through October
  filter(month %in% c(4, 5, 6, 7, 8, 9, 10)) |> 
  group_by(id, water_year) |> 
  # Calculate total growing season effective precipitation
  summarize(peff_grow_in = sum(peff_in), .groups = "drop")

# Join growing season effective precipitation with rest of data
masterdata = depletion_data1 |> 
  left_join(depletion_data2, by = c("id", "water_year")) |> 
  # Select needed variables
  select(
    id,
    year,
    month,
    acres,
    et_in,
    peff_in,
    smco_in
  )

# ==== SAVE ====================================================================

save(masterdata, file = "Data/Clean/Depletion/Utah/masterdata.rda")
