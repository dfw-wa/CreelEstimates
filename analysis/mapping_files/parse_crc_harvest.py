"""
Parse Heidi's CRC-expanded freshwater salmon harvest workbooks into tidy form.

Sheet layout (one sheet per license year, "FW YYYY-YYYY", Apr 1 - Mar 31):

  A Region   B System   C Stream   D Code   E Species   F..R Apr..Mar   S Total

Three structures here will silently corrupt a naive read, and each is handled:

1. REGION LIVES ON SECTION HEADER ROWS ONLY. Region text sits in column A of a
   repeated header row (B=="System", C=="Stream"). Filling column A downward from
   the first data row stamps "Coastal" on the entire sheet - this is the known
   region-label defect. Region is instead latched from each header row.

2. SUBTOTAL BLOCKS ARE STRUCTURALLY IDENTICAL TO DATA. Region subtotals appear as
   B=="Total" with an empty Code, and the grand total as A=="Total - All Areas:".
   Filling Code downward makes them inherit the previous stream's code and roughly
   doubles that stream's harvest. Blocks are latched on a non-empty Code and
   released on a subtotal marker instead.

3. PER-STREAM "Total" SPECIES ROWS. Every stream block ends with Species=="Total",
   which is the sum of the rows above it. Kept as a checksum, excluded from output.

License year is retained, and each month is also mapped to its calendar year so
harvest can be compared against calendar-year creel estimates.

Output: crc_harvest_tidy.csv  (one row per file x code x species x month)
"""
import openpyxl
import pandas as pd
import re
import os

FILES = [
    # (path, sheet, license_year_label, calendar year of Apr..Dec)
    ("/mnt/project/Salmon_Freshwater_Estimates_2023.xlsx", "FW 2023-2024", "2023-24", 2023),
    ("/mnt/project/Salmon_Freshwater_Estimates_2024_Final.xlsx", "FW 2024-2025", "2024-25", 2024),
]

MONTHS = ["Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
          "Jan", "Feb", "Mar"]
MONTH_NUM = {"Jan": 1, "Feb": 2, "Mar": 3, "Apr": 4, "May": 5, "Jun": 6, "Jul": 7,
             "Aug": 8, "Sep": 9, "Oct": 10, "Nov": 11, "Dec": 12}


def norm(v):
    if v is None:
        return ""
    return re.sub(r"\s+", " ", str(v)).strip()


def parse(path, sheet, lic_year, y1):
    ws = openpyxl.load_workbook(path, data_only=True)[sheet]
    rows = list(ws.iter_rows(values_only=True))

    region = ""
    system = ""
    stream = ""
    code = ""
    in_block = False          # inside a real stream block?
    out, checks = [], []

    for r in rows:
        a, b, c, d, e = (norm(r[i]) for i in range(5))

        if a.startswith("Total - All Areas"):
            break                                    # grand total: stop
        if b == "System" and c == "Stream":
            # The very first header row is the literal column header ("Region");
            # that section's real name sits in column A of its first DATA row.
            if a and a != "Region":
                region = re.sub(r"\s*-\s*", " - ", a.replace("\n", " ")).strip()
            in_block = False                         # header: new region section
            continue
        if b == "Total":
            in_block = False                         # region subtotal: drop block
            continue
        if not e:
            continue

        if d:                                        # new stream block starts here
            if a and a != "Region":                  # region named on a data row
                region = re.sub(r"\s*-\s*", " - ", a.replace("\n", " ")).strip()
            code, stream = d, c
            if b:
                system = b
            in_block = True
        if not in_block:
            continue                                 # orphan rows under a subtotal

        vals = {m: r[5 + i] for i, m in enumerate(MONTHS)}
        total = r[17]

        if e.lower() == "total":
            checks.append({"code": code, "stream_total": total})
            continue

        for m, v in vals.items():
            if v in (None, "", 0):
                continue
            out.append({
                "source_file": os.path.basename(path),
                "license_year": lic_year,
                "region_crc": region,
                "system": system,
                "stream": stream,
                "catch_record_code": code,
                "species": e,
                "month": MONTH_NUM[m],
                "year": y1 if MONTH_NUM[m] >= 4 else y1 + 1,
                "harvest": float(v),
            })
    return pd.DataFrame(out), pd.DataFrame(checks)


frames, checkframes = [], []
for path, sheet, lic, y1 in FILES:
    df, ck = parse(path, sheet, lic, y1)
    # checksum: parsed species rows must reproduce each stream's Total row
    got = df.groupby("catch_record_code").harvest.sum()
    want = ck.groupby("code").stream_total.sum()
    cmp = pd.concat([got.rename("parsed"), want.rename("sheet")], axis=1).fillna(0)
    bad = cmp[(cmp.parsed - cmp.sheet).abs() > 1]
    print(f"{os.path.basename(path)}: {len(df):,} rows, {df.catch_record_code.nunique()} codes, "
          f"{df.harvest.sum():,.0f} fish | checksum mismatches: {len(bad)}")
    if len(bad):
        print(bad.head(10).to_string())
    frames.append(df)
    checkframes.append(ck)

h = pd.concat(frames, ignore_index=True)
h.to_csv("crc_harvest_tidy.csv", index=False)

print("\nregions found:")
print(h.groupby("region_crc").agg(codes=("catch_record_code", "nunique"),
                                  fish=("harvest", "sum")).round(0).to_string())
print("\nby calendar year:")
print(h.groupby("year").agg(codes=("catch_record_code", "nunique"),
                            fish=("harvest", "sum")).round(0).to_string())
