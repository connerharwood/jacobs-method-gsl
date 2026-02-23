
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/fields_panel.gpkg") |> 
  st_drop_geometry()

# Load masterdata
load("Data/Clean/masterdata.rda")

# ==== CALCULATE DEPLETION =====================================================

depletion_monthly = masterdata |> 
  filter(month %in% 4:10) |> 
  arrange(id, year, month) |> 
  mutate(
    depletion_in = pmax(0, et_in - smco_in - peff_in),
    depletion_ft = depletion_in / 12,
    depletion_af = depletion_ft * acres
  ) |> 
  ungroup() |> 
  left_join(
    fields_panel,
    by = c("id", "year"),
    relationship = "many-to-one"
  ) |> 
  select(
    id,
    year,
    month,
    county,
    basin,
    sub_area,
    land_use,
    land_use_group,
    crop,
    crop_group,
    irr_method,
    acres = acres.x,
    et_in,
    peff_in,
    smco_in,
    depletion_in,
    depletion_ft,
    depletion_af
  )

depletion_annual = depletion_monthly |> 
  group_by(id, year) |> 
  summarize(
    et_in_grow = sum(et_in, na.rm = FALSE),
    peff_in_grow = sum(peff_in, na.rm = FALSE),
    smco_in_april = first(smco_in),
    depletion_in = sum(depletion_in, na.rm = FALSE),
    depletion_ft = sum(depletion_ft, na.rm = FALSE), 
    depletion_af = sum(depletion_af, na.rm = FALSE),
    .groups = "drop"
  ) |> 
  ungroup() |> 
  left_join(
    fields_panel,
    by = c("id", "year"),
    relationship = "one-to-one"
  ) |> 
  select(
    id,
    year,
    county,
    basin,
    sub_area,
    land_use,
    land_use_group,
    crop,
    crop_group,
    irr_method,
    acres,
    et_in_grow,
    peff_in_grow,
    smco_in_april,
    depletion_in,
    depletion_ft,
    depletion_af
  )

median(depletion_annual$depletion_ft, na.rm = TRUE) # 1.486835
median(depletion_annual$depletion_af, na.rm = TRUE) # 7.299035

# ==== SAVE ====================================================================

save(depletion_monthly, file = "Data/Clean/depletion_monthly.rda")
save(depletion_annual, file = "Data/Clean/depletion_annual.rda")
