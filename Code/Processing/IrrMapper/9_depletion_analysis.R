
library(tidyverse)

# ==== LOAD ====================================================================

load("Data/Clean/Depletion/Utah/depletion_monthly.rda")
load("Data/Clean/Depletion/Utah/depletion_annual.rda")

# ==== ANNUAL MEDIAN ===========================================================

cultivated = depletion_annual |> 
  filter(!crop %in% c(
    "Developed/Open Space",
    "Fallow/Idle Cropland",
    "Developed/Low Intensity",
    "Shrubland",
    "Herbaceous Wetlands",
    "Woody Wetlands",
    "Barren",
    "Developed/Med Intensity",
    "Evergreen Forest",
    "Deciduous Forest",
    "Mixed Forest"
  ))
median(cultivated$depletion_ft, na.rm = TRUE)

uncultivated = depletion_annual |> 
  filter(crop %in% c(
    "Developed/Open Space",
    "Fallow/Idle Cropland",
    "Developed/Low Intensity",
    "Shrubland",
    "Herbaceous Wetlands",
    "Woody Wetlands",
    "Barren",
    "Developed/Med Intensity",
    "Evergreen Forest",
    "Deciduous Forest",
    "Mixed Forest"
  ))
median(uncultivated$depletion_ft, na.rm = TRUE)

fallow = depletion_annual |> filter(crop == "Fallow/Idle Cropland")
median(fallow$depletion_ft, na.rm = TRUE)

irrigated = depletion_annual |> filter(irr_method != "Dry Crop", crop != "Fallow/Idle Cropland")
median(irrigated$depletion_ft, na.rm = TRUE)


fallow_acres = fallow |> 
  filter(year == 2024)
sum(fallow_acres$acres)
alfalfa = depletion_annual |> filter(crop == "Alfalfa")
median(alfalfa$et_in, na.rm = TRUE)

crop_et = depletion_annual |> 
  group_by(crop) |> 
  summarize(et_in = mean(et_in, na.rm = TRUE), .groups = "drop")
