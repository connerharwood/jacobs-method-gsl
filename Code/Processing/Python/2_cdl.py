
# %%
import os
import re
import pandas as pd
import numpy as np
import geopandas as gpd
import rasterio
from exactextract import exact_extract

# %% LOAD
# Load crop rooting zone depths
rooting_depth = pd.read_excel("Data/Misc/rooting_depth.xlsx", sheet_name = "wrlu")

# Load CDL codes
cdl_codes = pd.read_excel("Data/Misc/rooting_depth.xlsx", sheet_name="cdl")[["cdl_code", "crop"]]

# Load sample rast to transform fields CRS
sample_rast = rasterio.open("Data/Raw/CDL/Utah/CDL_2024_49.tif")

# Load temporary fields panel, align with sample rast CRS
fields_panel_temp = gpd.read_file("Data/Python/fields_panel_temp.gpkg").to_crs(sample_rast.crs)

# Convert field ID to int
fields_panel_temp["id"] = fields_panel_temp["id"].astype(int)

# Filter to fields missing crop info
fields_crop_na = fields_panel_temp[fields_panel_temp["crop"].isna()].copy()

# List of yearly CDL TIFF files
tiff_folder = "Data/Raw/CDL/Utah"
tiff_files = [
    os.path.join(tiff_folder, f) 
    for f in os.listdir(tiff_folder)
    if f.lower().endswith((".tif"))
]

# Initialize dictionary store each year
cdl_panel = {}

# %%
# Loop through and process each TIFF file
for file in tiff_files:
    print(f"Processing {file}")

    # Extract year from TIFF file
    current_year = int(re.search(r"\d{4}", file).group())

    # Filter fields to current year
    fields_current = fields_crop_na[fields_crop_na["year"] == current_year].copy()

    # Load CDL TIFF file
    cdl_rast = rasterio.open(file)

    # Extract mode crop for each field
    cdl_extract = exact_extract(
        # Extract raster values for current year's field polygons
        cdl_rast,
        fields_current,

        # Area-weighted majority crop over a field's pixels
        "majority",

        # Return output as a pandas dataframe
        output = "pandas"
    )

    cdl_fields = pd.DataFrame({
        "id": fields_current["id"].values,
        "year": current_year,
        "cdl_code": cdl_extract["majority"].values.astype(int)
    })

    # Store current year in dictionary
    cdl_panel[str(current_year)] = cdl_fields

# %%
# Merge all CDL years
cdl_df = pd.concat(cdl_panel.values(), ignore_index=True)

# %%
# Merge with fields panel
fields_panel = fields_panel_temp.merge(
    cdl_df,
    on=["id", "year"],
    how="left"
).merge(
    cdl_codes,
    on=["cdl_code"],
    how="left"
)

# %%
cdl_harmonized = fields_panel.copy()

# %%
cdl_harmonized["crop_y"] = np.select(
    [
        cdl_harmonized["crop_y"] == "Alfalfa",
        cdl_harmonized["crop_y"] == "Apples",
        cdl_harmonized["crop_y"] == "Apricots",
        cdl_harmonized["crop_y"] == "Barley",
        cdl_harmonized["crop_y"] == "Barren",
        cdl_harmonized["crop_y"] == "Cherries",
        cdl_harmonized["crop_y"] == "Chick Peas",
        cdl_harmonized["crop_y"] == "Corn",
        cdl_harmonized["crop_y"] == "Dbl Crop Triticale/Corn",
        cdl_harmonized["crop_y"] == "Dbl Crop WinWht/Corn",
        cdl_harmonized["crop_y"] == "Deciduous Forest",
        cdl_harmonized["crop_y"] == "Developed/Low Intensity",
        (cdl_harmonized["crop_y"] == "Developed/Med Intensity") & (cdl_harmonized["year"].isin([2017, 2018, 2019, 2020, 2022])),
        (cdl_harmonized["crop_y"] == "Developed/Med Intensity") & (cdl_harmonized["year"].isin([2021, 2023, 2024])),
        cdl_harmonized["crop_y"] == "Developed/Open Space",
        cdl_harmonized["crop_y"] == "Dry Beans",
        cdl_harmonized["crop_y"] == "Evergreen Forest",
        cdl_harmonized["crop_y"] == "Fallow/Idle Cropland",
        cdl_harmonized["crop_y"] == "Flaxseed",
        cdl_harmonized["crop_y"] == "Grass/Pasture",
        cdl_harmonized["crop_y"] == "Herbaceous Wetlands",
        cdl_harmonized["crop_y"] == "Herbs",
        cdl_harmonized["crop_y"] == "Misc Vegs & Fruits",
        cdl_harmonized["crop_y"] == "Mixed Forest",
        cdl_harmonized["crop_y"] == "Mustard",
        cdl_harmonized["crop_y"] == "Oats",
        cdl_harmonized["crop_y"] == "Onions",
        cdl_harmonized["crop_y"] == "Other Crops",
        cdl_harmonized["crop_y"] == "Other Hay/Non Alfalfa",
        cdl_harmonized["crop_y"] == "Peaches",
        cdl_harmonized["crop_y"] == "Pears",
        cdl_harmonized["crop_y"] == "Peas",
        cdl_harmonized["crop_y"] == "Potatoes",
        cdl_harmonized["crop_y"] == "Pumpkins",
        cdl_harmonized["crop_y"] == "Rye",
        cdl_harmonized["crop_y"] == "Safflower",
        cdl_harmonized["crop_y"] == "Shrubland",
        cdl_harmonized["crop_y"] == "Sod/Grass Seed",
        cdl_harmonized["crop_y"] == "Sorghum",
        cdl_harmonized["crop_y"] == "Spring Wheat",
        cdl_harmonized["crop_y"] == "Squash",
        cdl_harmonized["crop_y"] == "Sunflower",
        cdl_harmonized["crop_y"] == "Sweet Corn",
        cdl_harmonized["crop_y"] == "Triticale",
        cdl_harmonized["crop_y"] == "Watermelons",
        cdl_harmonized["crop_y"] == "Winter Wheat",
        cdl_harmonized["crop_y"] == "Woody Wetlands"
    ],
    [
        "Alfalfa", "Apples", "Apricots", "Barley", "Fallow/Idle", "Cherries",
        "Beans", "Corn", "Corn", "Corn", "Pasture", "Pasture", "Pasture", 
        "Vegetables", "Pasture", "Beans", "Pasture", "Fallow/Idle", "Flaxseed",
        "Grass Hay", "Pasture", "Horticulture", "Vegetables", "Pasture", "Mustard",
        "Oats", "Onion", "Field Crop Unspecified", "Grass Hay", "Peaches", "Peaches",
        "Vegetables", "Potato", "Pumpkins", "Rye", "Safflower", "Fallow/Idle",
        "Grass Hay", "Sorghum", "Spring Wheat", "Squash", "Sunflower", "Corn",
        "Triticale", "Watermelons", "Winter Wheat", "Pasture"
    ],
    default=cdl_harmonized["crop_y"]
)

# %%
cdl_harmonized["crop"] = np.where(
    cdl_harmonized["crop_x"].isna(),
    cdl_harmonized["crop_y"],
    cdl_harmonized["crop_x"]
)

fields_panel = cdl_harmonized.merge(
    rooting_depth,
    on="crop",
    how="left"
)[[
    "id",
    "year",
    "county",
    "basin",
    "sub_area",
    "land_use",
    "acres",
    "cultivated",
    "crop",
    "crop_group",
    "land_use_group",
    "rz_in",
    "irr_method",
    "geometry"
]].to_crs(epsg=26912)

# %%
fields_panel.to_file("Data/Python/fields_panel.gpkg")
