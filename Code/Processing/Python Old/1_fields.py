
# %%
import geopandas as gpd
import pandas as pd
from pathlib import Path
import numpy as np
import pandas as pd
import re

# %%
# Initialize dictionary to store each WRLU year
wrlu_dict = {}

for yr in range(2017, 2025):

    # Define shapefile path
    file_path = Path(f"Data/Raw/Fields/Utah/Repaired WRLU/{yr}/wrlu_{yr}.shp")

    if yr == 2024:
        wrlu_2024 = gpd.read_file(file_path)

        wrlu_2024 = wrlu_2024[
            (wrlu_2024["Landuse"] == "Agricultural") &
            (wrlu_2024["State"] == "Utah")
        ]

        # Select and rename needed variables
        wrlu_2024 = wrlu_2024[[
            "OBJECTID", "County", "Basin", "SubArea", "Landuse", "Acres", "Descriptio", 
            "Class_Name", "CropGroup", "LU_Group", "IRR_Method", "geometry"
        ]].rename(columns={
            "OBJECTID": "id", 
            "County": "county", 
            "Basin": "basin", 
            "SubArea": "sub_area", 
            "Landuse": "land_use",
            "Acres": "acres_2024", 
            "Descriptio": "crop_2024", 
            "Class_Name": "cdl_2024",
            "CropGroup": "crop_group_2024", 
            "LU_Group": "land_use_group_2024", 
            "IRR_Method": "irr_method_2024"
        })

        # Make geometry valid
        wrlu_2024["geometry"] = wrlu_2024["geometry"].make_valid()

        # Transform to NAD 83 for spatial operations
        wrlu_2024 = wrlu_2024.to_crs(epsg=26912)

    else:
        # Load shapeilfe
        wrlu = gpd.read_file(file_path)

        # Only include ag fields in Utah
        wrlu = wrlu[
            (wrlu["Landuse"] == "Agricultural") &
            (wrlu["State"] == "Utah")
        ]

        # Select and rename needed columns
        wrlu = wrlu[[
            "Acres", "Descriptio", "Class_Name", "CropGroup", "LU_Group", "IRR_Method", "geometry"
        ]].rename(columns={
            "Acres": f"acres_{yr}",
            "Descriptio": f"crop_{yr}",
            "Class_Name": f"cdl_{yr}",
            "CropGroup": f"crop_group_{yr}",
            "LU_Group": f"land_use_group_{yr}",
            "IRR_Method": f"irr_method_{yr}"
        })

        # Make geometry valid
        wrlu["geometry"] = wrlu["geometry"].make_valid()

        # Transform to NAD 83 for spatial operations
        wrlu = wrlu.to_crs(epsg=26912)

        # Store current WRLU year in dictionary
        wrlu_dict[str(yr)] = wrlu

# %% BUILD PANEL

# Define 2024 fields as base polygons
wrlu_base = wrlu_2024.copy()

# %%
for yr in range(2017, 2024):
    # Get current year's fields
    wrlu_current = wrlu_dict[str(yr)]
    
    # Join current year's fields with 2024 fields
    wrlu_base = gpd.sjoin(
        wrlu_base.set_crs(crs=wrlu_current.crs),
        wrlu_current,
        how = "left",
        predicate = "intersects"
    )

    # Create dynamic columns for each year
    acres_col = f"acres_{yr}"
    crop_col = f"crop_{yr}"
    cdl_col = f"cdl_{yr}"
    crop_group_col = f"crop_group_{yr}"
    land_use_group_col = f"land_use_group_{yr}"
    irr_method_col = f"irr_method_{yr}"

    # Calculate difference in acres between current year and 2024 field
    wrlu_base["acres_diff"] = abs(wrlu_base["acres_2024"] - wrlu_base[acres_col])

    # For each field, keep the intersecting field with smallest difference in acreage
    wrlu_base = (wrlu_base
                 .sort_values("acres_diff")
                 .groupby("id", as_index = False)
                 .first())
    
    # Replace current year's entry with NA if acreage difference is too big
    wrlu_base[crop_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                   np.nan,
                                   wrlu_base[crop_col])
    
    wrlu_base[cdl_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                  np.nan,
                                  wrlu_base[cdl_col])
    
    wrlu_base[crop_group_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                         np.nan,
                                         wrlu_base[crop_group_col])
    
    wrlu_base[land_use_group_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                             np.nan,
                                             wrlu_base[land_use_group_col])
    
    wrlu_base[irr_method_col] = np.where(wrlu_base["acres_diff"] > 0.01,
                                         np.nan,
                                         wrlu_base[irr_method_col])

    # Remove index column
    if "index_right" in wrlu_base.columns:
        wrlu_base = wrlu_base.drop(columns=["index_right"])

#%%
wrlu_panel = pd.wide_to_long(
    df = wrlu_base,
    stubnames = ["crop", "crop_group", "cdl", "land_use_group", "irr_method"],
    i = [col for col in wrlu_base.columns if col not in [col for col in wrlu_base.columns if re.match(r'^(crop(_group)?|cdl|land_use_group|irr_method)_', col)]],
    j = "year",
    sep = "_",
    suffix = r"\d{4}"
).reset_index()

# %%
wrlu_harmonized = wrlu_panel.copy()

# %%
grass_hay_fields = wrlu_panel[
    (wrlu_panel["year"] == 2024) & 
    (wrlu_panel["crop"] == "Grass Hay")
]

#%%
wrlu_harmonized["crop"] = np.select(
    [
        # Overwrite 2017-2023 Alfalfa crop with Grass Hay if 2024 crop was Grass Hay
        (wrlu_panel["id"].isin(grass_hay_fields["id"])) &
        (wrlu_panel["year"] < 2024) &
        (wrlu_panel["crop"] == "Alfalfa"),

        # Put Fallow, Idle, and Idle Pasture into one category
        wrlu_panel["crop"].isin(["Fallow", "Idle", "Idle Pasture"]),

        # Put Turfgrass into Turfgrass Ag
        wrlu_panel["crop"] == "Turfgrass",

        # Respell Grassy Hay to Grass Hay
        wrlu_panel["crop"] == "Grassy Hay",

        # Align CDL name with WRLU name
        wrlu_panel["crop"] == "Grassland/Pasture",

        # Set Dry Land/Other as Fallow/Idle
        wrlu_panel["crop"] == "Dry Land/Other"
    ],
    [
        "Grass Hay",
        "Fallow/Idle",
        "Turfgrass Ag",
        "Grass Hay",
        "Grass/Pasture",
        "Fallow/Idle"
    ],
    default = wrlu_panel["crop"]
)
# %%
# Finalize temporary panel
fields_panel_temp = (
    wrlu_harmonized[[
        "id", "year", "county", "basin", "sub_area", "land_use", "acres_2024",
        "crop", "crop_group", "land_use_group", "irr_method", "geometry"
    ]]
    .rename(columns={"acres_2024": "acres"})
    .sort_values(by = ["id", "year"], ascending= [ True, False])
    .set_crs(epsg=26912)
)

# %% SAVE 
fields_panel_temp.to_file("Data/Python/fields_panel_temp.gpkg")
