
# %%
import os
import pandas as pd
import geopandas as gpd

ssurgo_folder = "Data/Raw/SSURGO/Utah"

# %% APPEND MAP UNIT POLYGONS
mu_poly_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(ssurgo_folder)
    for f in files
    if f.lower().startswith("soilmu_a_") and f.lower().endswith(".shp")
]

mu_poly_list = []

for file in mu_poly_files:
    gdf = gpd.read_file(file)

    # Make geometry valid
    gdf["geometry"] = gdf["geometry"].make_valid()

    # Reproject to NAD 83 for spatial operations
    gdf = gdf.to_crs(epsg=26912)

    # Select and rename needed variables
    gdf = gdf[["MUKEY", "AREASYMBOL", "geometry"]].rename(
        columns={"MUKEY": "mukey", "AREASYMBOL": "survey_area"}
    )

    # Convert map unit key to string
    gdf["mukey"] = gdf["mukey"].astype(str)

    # Append to list
    mu_poly_list.append(gdf)

mu_polys = gpd.GeoDataFrame(pd.concat(mu_poly_list, ignore_index=True), crs=26912)

mu_polys.to_file("Data/Python/ssurgo_ut.gpkg", layer="mu_polys", driver="GPKG")

# %% APPEND COMPONENT DATA
comp_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(ssurgo_folder)
    for f in files
    if f.lower() == "comp.txt"
]

# Identify and skip empty files
nonempty_files = []
for f in comp_files:
    try:
        df = pd.read_csv(f, sep = "|", header=None, quotechar='"', engine="python")
        if len(df) > 0:
            nonempty_files.append(f)
    except Exception:
        continue

# Read and append component data
comp_list =[]
for f in nonempty_files:
    df = pd.read_csv(f, sep = "|", header=None, quotechar='"', engine="python")

    comp_df = pd.DataFrame({
        "mukey": df.iloc[:, 107].astype(str),
        "cokey": df.iloc[:, 108].astype(str),
        "co_pct": df.iloc[:, 1] / 100
    })

    comp_df["co_pct_sum"] = comp_df.groupby("mukey")["co_pct"].transform("sum")
    comp_df["co_pct"] = comp_df.apply(
        lambda row: row["co_pct"] / row["co_pct_sum"] if row["co_pct_sum"] < 1 else row["co_pct"],
        axis=1
    )
    comp_df = comp_df.drop(columns="co_pct_sum")

    comp_list.append(comp_df)

comp = pd.concat(comp_list, ignore_index=True)

gpd.GeoDataFrame(comp).to_file("Data/Python/ssurgo_ut.gpkg", layer="comp", driver="GPKG")

# %% APPEND COMPONENT RESTRICTIONS DATA
co_restrictions_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(ssurgo_folder)
    for f in files
    if f.lower() == "crstrcts.txt"
]

# Identify and skip empty files
nonempty_files = []
for f in co_restrictions_files:
    try:
        df = pd.read_csv(f, sep = "|", header=None, quotechar='"', engine="python")
        if len(df) > 0:
            nonempty_files.append(f)
    except Exception:
        continue

# Read and append component restrictions data
co_restrictions_list = []
for f in nonempty_files:
    df = pd.read_csv(f, sep="|", header=None, quotechar='"', engine="python")

    co_restrictions_df = pd.DataFrame({
        "cokey": df.iloc[:, 11].astype(str),
        "restrictive_layer_cm": df.iloc[:, 3].astype(float)
    })

    co_restrictions_list.append(co_restrictions_df)

co_restrictions = pd.concat(co_restrictions_list, ignore_index=True)

gpd.GeoDataFrame(co_restrictions).to_file("Data/Python/ssurgo_ut.gpkg", layer="co_restrictions", driver="GPKG")

# %% APPEND MAP UNIT AGGATT DATA
mu_aggatt_files = [
    os.path.join(root, f)
    for root, _, files in os.walk(ssurgo_folder)
    for f in files
    if f.lower() == "muaggatt.txt"
]

# Identify and skip empty files
nonempty_files = []
for f in mu_aggatt_files:
    try:
        df = pd.read_csv(f, sep="|", header=None, quotechar='"', engine="python")
        if len(df) > 0:
            nonempty_files.append(f)
    except Exception:
        continue

# Read and append muaggatt data
mu_aggatt_list = []
for f in nonempty_files:
    df = pd.read_csv(f, sep="|", header=None, quotechar='"')

    mu_aggatt_df = pd.DataFrame({
        "mukey": df.iloc[:, 39].astype(str),
        "aws_cm": df.iloc[:, 14].astype(float),
        "water_table_cm": df.iloc[:, 7].astype(float)
    })

    mu_aggatt_list.append(mu_aggatt_df)

mu_aggatt = pd.concat(mu_aggatt_list, ignore_index=True)

gpd.GeoDataFrame(mu_aggatt).to_file("Data/Python/ssurgo_ut.gpkg", layer="mu_aggatt", driver="GPKG")

# %%
