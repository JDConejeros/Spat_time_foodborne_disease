"""
Daily ERA5-Land (GEE) extraction: temperature + precipitation over Chilean commune polygons.

Source geometries: 2-Data/Output/Population_municipality_2011_2024.RData (unique comunas).

Bands (ECMWF/ERA5_LAND/DAILY_AGGR):
  - temperature_2m, temperature_2m_min, temperature_2m_max  -> Celsius (K - 273.15)
  - total_precipitation_sum, total_precipitation_min, total_precipitation_max -> meters (depth)

Period: 2011-01-01 <= date < 2025-01-01 (daily).

Exports one table to Google Drive per calendar year (EE batch limits).
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import ee
import geopandas as gpd

# Project root = parent of 1-Scripts
ROOT = Path(__file__).resolve().parent.parent
GEOJSON_OUT = ROOT / "2-Data" / "Output" / "Population_municipality_comunas.geojson"
RDATA_PATH = ROOT / "2-Data" / "Output" / "Population_municipality_2011_2024.RData"

GEE_PROJECT = os.environ.get("GEE_PROJECT", "quadrant-rm")
DRIVE_FOLDER = os.environ.get("EE_EXPORT_FOLDER", "EarthEngine_Exports")

START_DATE = "2011-01-01"
END_DATE = "2025-01-01"

ERA5_COLLECTION = "ECMWF/ERA5_LAND/DAILY_AGGR"
BANDS = [
    "temperature_2m",
    "temperature_2m_min",
    "temperature_2m_max",
    "total_precipitation_sum",
    "total_precipitation_min",
    "total_precipitation_max",
]
SCALE_M = 11132  # ERA5-Land nominal ~9–11 km; dataset uses 11132 m in band table


def export_commune_geojson_from_r() -> None:
    """Write unique-comuna polygons to GeoJSON from the project RData."""
    if (
        GEOJSON_OUT.is_file()
        and os.environ.get("FORCE_REEXPORT_GEOJSON", "").lower() not in ("1", "true", "yes")
    ):
        print(f"Using existing GeoJSON (set FORCE_REEXPORT_GEOJSON=1 to rebuild): {GEOJSON_OUT}")
        return

    r_code = f"""
    load("{RDATA_PATH.as_posix()}")
    library(dplyr)
    library(sf)
    comunas_geo <- pop_mun |>
      dplyr::group_by(comuna) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(comuna, name_com, geometry)
    if (is.na(sf::st_crs(comunas_geo))) {{
      comunas_geo <- sf::st_set_crs(comunas_geo, 4326)
    }} else {{
      comunas_geo <- sf::st_transform(comunas_geo, 4326)
    }}
    sf::st_write(
      comunas_geo,
      "{GEOJSON_OUT.as_posix()}",
      delete_dsn = TRUE,
      quiet = TRUE
    )
    """
    import subprocess

    subprocess.run(
        ["Rscript", "-e", r_code],
        cwd=str(ROOT),
        check=True,
    )
    print(f"Wrote {GEOJSON_OUT}")


def gdf_to_ee_featurecollection(gdf: gpd.GeoDataFrame) -> ee.FeatureCollection:
    """Convert GeoDataFrame to an Earth Engine FeatureCollection."""
    features = []
    for idx, row in gdf.iterrows():
        geom = row.geometry
        try:
            if geom.geom_type == "Polygon":
                coords = [[[p[0], p[1]] for p in geom.exterior.coords]]
                ee_geom = ee.Geometry.Polygon(coords)
            elif geom.geom_type == "MultiPolygon":
                polygons: list = []
                for poly in geom.geoms:
                    rings = [
                        [[p[0], p[1]] for p in poly.exterior.coords]
                    ]
                    for interior in poly.interiors:
                        rings.append([[p[0], p[1]] for p in interior.coords])
                    polygons.append(rings)
                ee_geom = ee.Geometry.MultiPolygon(polygons)
            else:
                print(f"Skipping geometry {idx}: type {geom.geom_type}")
                continue

            comuna = int(row["comuna"])
            name_com = row.get("name_com")
            if name_com is not None and hasattr(name_com, "item"):
                name_com = name_com.item()
            name_com = "" if name_com is None else str(name_com)

            feature = ee.Feature(
                ee_geom,
                {
                    "geometry_id": int(idx),
                    "comuna": comuna,
                    "name_com": name_com,
                    "codigo_comuna": comuna,
                },
            )
            features.append(feature)
        except Exception as e:
            print(f"Error processing geometry {idx}: {e}")
            continue

    return ee.FeatureCollection(features)


def extract_year_table(fc: ee.FeatureCollection, start: str, end: str) -> ee.FeatureCollection:
    """Map each daily image to one feature per commune (mean over polygon)."""

    era5 = (
        ee.ImageCollection(ERA5_COLLECTION)
        .filterDate(ee.Date(start), ee.Date(end))
        .select(BANDS)
    )

    def process_image(image: ee.Image) -> ee.FeatureCollection:
        date = ee.Date(image.get("system:time_start"))
        date_str = date.format("YYYY-MM-dd")

        def per_feature(feature: ee.Feature) -> ee.Feature:
            geom = feature.geometry()
            stats = image.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=geom,
                scale=SCALE_M,
                maxPixels=1e9,
            )

            def k_to_c(name: str) -> ee.ComputedObject:
                v = stats.get(name)
                return ee.Algorithms.If(
                    v, ee.Number(v).subtract(273.15), None  # type: ignore[arg-type]
                )

            props: dict = {
                "comuna": feature.get("comuna"),
                "codigo_comuna": feature.get("codigo_comuna"),
                "name_com": feature.get("name_com"),
                "geometry_id": feature.get("geometry_id"),
                "date": date_str,
                "temperature_2m": k_to_c("temperature_2m"),
                "temperature_2m_min": k_to_c("temperature_2m_min"),
                "temperature_2m_max": k_to_c("temperature_2m_max"),
                "total_precipitation_sum": stats.get("total_precipitation_sum"),
                "total_precipitation_min": stats.get("total_precipitation_min"),
                "total_precipitation_max": stats.get("total_precipitation_max"),
            }
            return ee.Feature(None, props)

        return fc.map(per_feature)

    return era5.map(process_image).flatten()


def year_windows() -> list[tuple[int, str, str]]:
    """(year, start_date, end_date) with end exclusive, inside [2011-01-01, 2025-01-01)."""
    out: list[tuple[int, str, str]] = []
    for y in range(2011, 2025):
        s = f"{y}-01-01"
        e = f"{y + 1}-01-01"
        if s >= END_DATE:
            break
        if e > END_DATE:
            e = END_DATE
        out.append((y, s, e))
    return out


def main() -> None:
    if not RDATA_PATH.is_file():
        print(f"Missing {RDATA_PATH}", file=sys.stderr)
        sys.exit(1)

    print("Exporting commune GeoJSON from R …", flush=True)
    export_commune_geojson_from_r()

    gdf = gpd.read_file(GEOJSON_OUT)
    if gdf.crs is None or str(gdf.crs).upper() not in ("EPSG:4326", "OGC:CRS84"):
        gdf = gdf.to_crs(4326)
    else:
        gdf = gdf.to_crs("EPSG:4326")

    print(f"Loaded {len(gdf)} commune geometries", flush=True)

    # Must initialize before any ee.Feature / FeatureCollection
    try:
        ee.Initialize(project=GEE_PROJECT)
    except Exception:
        ee.Authenticate()
        ee.Initialize(project=GEE_PROJECT)

    ee_fc = gdf_to_ee_featurecollection(gdf)
    n = ee_fc.size().getInfo()
    print(f"EE FeatureCollection size: {n}", flush=True)

    tasks = []
    for year, s, e in year_windows():
        desc = f"ERA5_temp_precip_communes_{year}"
        print(f"Preparing export: {desc}  ({s} to {e})", flush=True)
        table = extract_year_table(ee_fc, s, e)
        t = ee.batch.Export.table.toDrive(
            collection=table,
            description=desc,
            folder=DRIVE_FOLDER,
            fileFormat="CSV",
        )
        t.start()
        tasks.append((desc, t))
        print(f"  started task id: {t.id}  state: {t.state}", flush=True)

    print(
        "\nAll batch exports submitted to Google Drive folder:",
        DRIVE_FOLDER,
        flush=True,
    )
    print("Download CSVs from Drive when status is COMPLETED.", flush=True)


if __name__ == "__main__":
    main()
