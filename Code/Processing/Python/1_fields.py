
# %%
import pandas as pd
import geopandas as gpd
import numpy as np
import os

# %% LOAD

# Initialize dictionary to store each WRLU year
wrlu_dict = dict()

# Load and process each WRLU year
for yr in range(2017, 2025):
    # Shapefile path
    file_path = f"Data/Raw/Fields/Repaired WRLU/{yr}/wrlu_{yr}.shp"

    # Load and process 2024 separately
    if yr == 2024:
        # Load shapefile
        wrlu_2024 = gpd.read_file(file_path)

        # Clean
        wrlu_2024 = (
            wrlu_2024[
                (wrlu_2024["Landuse"] == "Agricultural") &
                (wrlu_2024["State"] == "Utah")
            ]
            .reset_index(drop=True)
            .assign(id = lambda df: df.index + 1)
            .rename(columns = {
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
            [[
                "id", 
                "county", 
                "basin", 
                "sub_area", 
                "land_use", 
                "acres_2024",
                "crop_2024", 
                "cdl_2024", 
                "land_use_group_2024", 
                "irr_method_2024", 
                "geometry" 
            ]]
        )

        # Validate geometry
        wrlu_2024["geometry"] = wrlu_2024["geometry"].make_valid()

        # Transform to NAD 83 for spatial operations
        wrlu_2024 = wrlu_2024.to_crs(epsg=26912)

    else:
        # Load shapefile
        wrlu = gpd.read_file(file_path)

        # Clean
        wrlu = (
            wrlu[
                (wrlu["Landuse"] == "Agricultural") &
                (wrlu["State"] == "Utah")
            ]
            .rename(columns = {
                "Acres": f"acres_{yr}",
                "Descriptio": f"crop_{yr}",
                "Class_Name": f"cdl_{yr}",
                "CropGroup": f"crop_group_{yr}",
                "LU_Group": f"land_use_group_{yr}",
                "IRR_Method": f"irr_method_{yr}"
            })
            [[
                f"acres_{yr}",
                f"crop_{yr}",
                f"cdl_{yr}",
                f"crop_group_{yr}",
                f"land_use_group_{yr}",
                f"irr_method_{yr}",
                "geometry"
            ]]
        )

        # Validate geometry
        wrlu["geometry"] = wrlu["geometry"].make_valid()

        # Transform to NAD 83 for spatial operations
        wrlu = wrlu.to_crs(epsg=26912)

        # Store in dictionary with name by year
        wrlu_dict[yr] = wrlu

# %% BUILD PANEL

# Define 2024 fields as base polygons
wrlu_base = wrlu_2024

# Loop through each year, spatially joining with 2024 fields
for yr in range(2017, 2024):
    # Get current year's WRLU gpdf
    wrlu_current = wrlu_dict[yr]

    # Intersect current year's fields with 2024 fields
    wrlu_base = gpd.sjoin(
        wrlu_base,
        wrlu_current,
        how="left",
        predicate="intersects"
    ).drop(columns = ["index_right"])
# %%
