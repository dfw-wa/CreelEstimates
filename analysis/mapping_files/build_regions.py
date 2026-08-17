"""
Build the CRC region grouping layer.

Region is WDFW's own taxonomy, taken from column A of Heidi's harvest workbooks
(Coastal, Columbia - Lower / Middle / Upper / Snake, Puget Sound). It is preferred
over the deliverable-block classifier because it is published, is what the harvest
tables are organised by, and reconciles exactly with the blocks on the two clean
cases (Coastal <-> WA Coast, Puget Sound <-> Puget Sound). The Columbia splits cut
a different axis: WDFW divides the river by reach, the blocks separate mainstem
from tributaries. Both are kept - region groups, block scopes.

Harvest names a region for ~127 of 242 CRCs. The rest are assigned by:
  1. same HydrologicSystem as a code the harvest file does name  (strong)
  2. explicit reach rules for the Columbia and Snake mainstems   (strong)
  3. block fallback for Coastal / Puget Sound                    (safe)
Every assignment records how it was made, so nothing is silently invented.

Outputs:
  crc_region_lookup.csv   code -> region, with assignment provenance
  crc_regions.geojson     dissolved region boundaries for the map grouping layer
"""
import geopandas as gpd
import pandas as pd
import numpy as np

crc = gpd.read_file("crc_mapping-main/CRC.gdb.zip", layer="CRCArea")
crc = crc.drop_duplicates(subset="CatchRecordCode").set_crs(2927, allow_override=True)
crc["CatchRecordCode"] = crc.CatchRecordCode.astype(str).str.strip()

blocks = pd.read_csv("crc_block_lookup.csv", dtype={"CatchRecordCode": str})
crc = crc.merge(blocks[["CatchRecordCode", "block"]], on="CatchRecordCode", how="left")

h = pd.read_csv("crc_harvest_tidy.csv", dtype={"catch_record_code": str})
known = (h[h.region_crc != "Unknown"]
         .groupby("catch_record_code").region_crc.agg(lambda s: s.mode().iat[0]))

crc["region"] = crc.CatchRecordCode.map(known)
crc["region_source"] = np.where(crc.region.notna(), "CRC harvest file", "")

# --- 2. explicit reach rules for the mainstems -------------------------------
# Columbia reach breaks follow WDFW's own region assignment of the coded reaches:
# 519-525 Lower, 527-531 Middle, 533-549 Upper (the file places 533 in Upper, not
# Middle as the dam sequence would suggest - the file wins). These run FIRST: the
# whole Columbia mainstem shares one HydrologicSystem, so system propagation would
# otherwise collapse every reach from Buoy 10 to Grand Coulee into one region.
REACH = {**{c: "Columbia - Lower" for c in ["519", "521", "523", "525"]},
         **{c: "Columbia - Middle" for c in ["527", "529", "531"]},
         **{c: "Columbia - Upper" for c in ["533", "534", "535", "536", "537", "539", "541",
                                            "543", "545", "547", "549"]},
         **{c: "Columbia - Snake" for c in ["640", "642", "644", "646", "648", "650"]}}
m = crc.region.isna() & crc.CatchRecordCode.isin(REACH)
crc.loc[m, "region"] = crc.loc[m, "CatchRecordCode"].map(REACH)
crc.loc[m, "region_source"] = "mainstem reach rule"

# --- 2. propagate within a hydrologic system ---------------------------------
sysmode = (crc.dropna(subset=["region"])
           .groupby("HydrologicSystem").region.agg(lambda s: s.mode().iat[0]))
fill = crc.region.isna() & crc.HydrologicSystem.map(sysmode).notna()
crc.loc[fill, "region"] = crc.loc[fill, "HydrologicSystem"].map(sysmode)
crc.loc[fill, "region_source"] = "same hydrologic system"

# --- 3. block fallback --------------------------------------------------------
BLOCK_REGION = {"WA Coast": "Coastal", "Puget Sound": "Puget Sound",
                "Lower Columbia tribs": "Columbia - Lower",
                "Upper Columbia / Snake": "Columbia - Upper",
                "Columbia mainstem": "Columbia - Middle",
                "Marine (out of scope)": "Marine"}
m = crc.region.isna()
crc.loc[m, "region"] = crc.loc[m, "block"].map(BLOCK_REGION)
crc.loc[m, "region_source"] = "block fallback"

assert crc.region.notna().all(), crc[crc.region.isna()].CatchRecordCode.tolist()

crc[["CatchRecordCode", "NameOfCRC", "HydrologicSystem", "block",
     "region", "region_source"]].to_csv("crc_region_lookup.csv", index=False)

# --- dissolved boundaries for the map grouping layer --------------------------
reg = (crc[["region", "geometry"]].dissolve(by="region", as_index=False))
reg["geometry"] = reg.geometry.buffer(0).simplify(600)
reg["n_crc"] = crc.groupby("region").size().reindex(reg.region).values
reg.to_crs(4326).to_file("crc_regions.geojson", driver="GeoJSON")

print(crc.groupby(["region", "region_source"]).size().to_string())
print("\nregion totals:")
print(crc.groupby("region").size().to_string())
