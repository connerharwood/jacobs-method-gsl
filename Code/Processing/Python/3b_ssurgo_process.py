
# %%
import os
import pandas as pd
import geopandas as gpd
import numpy as np

# %%
fields = gpd.read_file("Data/Python/fields_panel.gpkg").query("year == 2024")

mu_polys = gpd.read_file("Data/Python/ssurgo_ut.gpkg", layer="mu_polys")

comp = gpd.read_file("Data/Python/ssurgo_ut.gpkg", layer="comp")

co_restrictions = gpd.read_file("Data/Python/ssurgo_ut.gpkg", layer="co_restrictions")

mu_aggatt = gpd.read_file("Data/Python/ssurgo_ut.gpkg", layer="mu_aggatt")

# %%
ssurgo_aoi = gpd.overlay(mu_polys, fields, how="intersection")

# %%
mu_restrictions = co_restrictions.merge(
    comp,
    on="cokey",
    how="left"
).groupby("mukey", as_index=False).apply(
    lambda g: pd.Series({
        "restrictive_layer_in": (
            g["restrictive_layer_cm"] * g["co_pct"]).sum() / g["co_pct"].sum() / 2.54
            if not g["restrictive_layer_cm"].isna().any() else None
    })
)

# %%
