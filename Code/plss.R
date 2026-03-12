
library(sf)

# ==== LOAD ====================================================================

quarter_quarter_raw = st_read(
  "https://services1.arcgis.com/99lidPhWCzftIe9K/ArcGIS/rest/services/UtahPLSSQuarterQuarterSections_GCDB/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

quarter_raw = st_read(
  "https://services1.arcgis.com/99lidPhWCzftIe9K/ArcGIS/rest/services/UtahPLSSQuarterSections_GCDB/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

section_raw = st_read(
  "https://services1.arcgis.com/99lidPhWCzftIe9K/ArcGIS/rest/services/PLSSSections_GCDB/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

township_raw = st_read(
  "https://services1.arcgis.com/99lidPhWCzftIe9K/ArcGIS/rest/services/PLSSTownships_GCDB/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
) |> 
  st_make_valid() |> 
  st_transform(crs = 26912)

# ==== SAVE ====================================================================

st_write(quarter_quarter, "Data/Raw/PLSS/plss_layers.gpkg", layer = "quarter_quarter")
st_write(quarter, "Data/Raw/PLSS/plss_layers.gpkg", layer = "quarter")
st_write(section, "Data/Raw/PLSS/plss_layers.gpkg", layer = "section")
st_write(township, "Data/Raw/PLSS/plss_layers.gpkg", layer = "township")
