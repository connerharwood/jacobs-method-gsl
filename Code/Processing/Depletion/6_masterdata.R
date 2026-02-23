
library(tidyverse)
library(sf)
library(purrr)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/ssurgo.rda") # Soil data
load("Data/Clean/prism.rda") # Monthly precipitation data
load("Data/Clean/openet_eemetric.rda") # Monthly ET data

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
  ) |> 
  # Calculate monthly effective precipitation
  mutate(peff_in = pmax(0, swsf * (0.70917 * prcp_in ^ 0.82416 - 0.11556) * 10 ^ (0.02426 * et_in)))

rm(fields_panel, ssurgo, prism, openet_eemetric)
gc()

# ==== DEPLETION INPUTS ========================================================

# Compute growing season monthly soil moisture
smco = merge |>
  mutate(
    # Calculate max soil moisture allowed
    smco_cap_in = 0.75 * rz_in * awc_in_in,
    # Calculate winter carryover soil moisture in April
    smco_april_in = pmax(0, pmin(0.67 * (prcp_win_in - 1.25 * et_win_in), smco_cap_in))
  ) |> 
  # Filter to growing season months
  filter(month %in% 4:10) |>
  group_by(id, water_year) |>
  arrange(month, .by_group = TRUE) |>
  mutate(
    # Calculate soil moisture at start of each month
    smco_in = {
      # Function to calculate soil moisture from previous month's values
      step = function(prev_smco, i) {
        min(max(prev_smco - et_in[i - 1] + peff_in[i - 1], 0), smco_cap_in[i])
      }
      out = rep(NA_real_, n())
      out[1] = smco_april_in[1]
      # Apply carryover soil moisture calculation sequentially by month
      out[2:7] = accumulate(2:7, step, .init = out[1])[-1]
      out
    }
  ) |>
  ungroup() |>
  select(id, year, month, water_year, smco_in)

masterdata = merge |> 
  # Join with growing season soil moisture
  left_join(smco, by = c("id", "year", "month", "water_year")) |> 
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

save(masterdata, file = "Data/Clean/masterdata.rda")
