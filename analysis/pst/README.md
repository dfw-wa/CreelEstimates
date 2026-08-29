# PST freshwater angler-trips workstream

This directory produces freshwater recreational salmon **angler trips**,
broken out by Year × River × Mode (guided/unguided) × Location (bank/boat),
2022–2025, for the Pacific Salmon Commission's PST economic valuation
(consultant: Northern Economics / Melissa Errend; scope authority: Jim
Scott).

Geographic scope: Puget Sound freshwater, WA Coast freshwater, and
Washington Columbia tributaries (including mid-Columbia R3 programs).
**Columbia mainstem** (Buoy 10 / Lower Columbia / Bonneville–McNary) is
**out of scope for delivery** — those files were transmitted to WDFW by the
consultant via ODFW, so they are not redelivered; the block stays in the
crosswalk for documentation only. **Steelhead is excluded** in every region.
Full scope decisions, requested format, and open questions are in
`01_intro_methods/_01_scope_and_contract.qmd`.

This is a standalone analysis workstream, separate from the in-season creel
reporting (`fw_creel.Rmd` and friends) documented in the repo root README.
Nothing here feeds, or is fed by, that machinery.

## Units caveat — read this before using any output

Every number this pipeline produces is an angler **trip** count
(`total_trips_est = total_effort_hrs / mean_trip_length`). The consultant's
template asks for "angler **days**." These are not interchangeable, and
**no trips→days conversion is applied anywhere in this repo**: some
fisheries run multi-day trips, some anglers take more than one trip in a
day, and the correct conversion (if one is even needed) is a decision to
work out with Jim/Melissa, not a pipeline assumption. Every script, column,
and output file here says `trips`, never `days`. See the Glossary section
of `pst_fw_angler_trips.qmd` (`# Glossary & unit conventions`) for the
canonical wording — stay consistent with it rather than paraphrasing.

## Before you run this

`analysis/pst/03_analysis/pst_fw_angler_trips_assembly.R` (pipeline step 5) reads
two governing files:

- `input_files/pst/lookup_tables/pst_input_manifest.csv`
- `input_files/pst/lookup_tables/pst_river_block_crosswalk.csv`

Both are now committed. They were absent for most of this workstream's
life, which is why the assembly script reads them through `read_if()` and
logs a gap rather than aborting: without the crosswalk every ingested row
is coded `block = "unknown"` and P2 expansion cannot run, and without the
manifest the blocking-source check is skipped. That degradation path is
deliberate (design rule R2) and worth keeping — but it means a run that
silently produces `block = "unknown"` everywhere is a signal to check that
these two files are actually being found, not a result to use as-is.

Everything else the pipeline reads lives under `input_files/pst/`,
organized by provider: `CRC/`, `R1_creel/`, `R3_creel/`, `external_data/`,
and `lookup_tables/`.

## Directory map

| Path | Contents |
|---|---|
| `01_intro_methods/` | `_`-prefixed Quarto fragments owning the framework: deliverable contract & scope (`_01_scope_and_contract.qmd`), the P1/P2/P3 tier hierarchy and Track A/B split (`_02_framework.qmd`), and the pipeline registry (`_03_pipeline_and_registry.qmd`). Never rendered standalone. |
| `02_ingest/` | Producer scripts that turn raw sources (DB, CRC workbooks, ad hoc R3 spreadsheets, interview records) into the tidy CSVs that `03_analysis/` reads. One is a `.qmd` (`interview_proportions.qmd`); the rest are `.R`. |
| `03_analysis/` | The two assembly scripts (`pst_fw_angler_trips_assembly.R`, `pst_fw_build_jim_workbook.R`), the P2/P3 helpers they source (`pst_p2_block_ratio.R`, `pst_crc_harvest_projection.R`), and the `_`-prefixed status/analysis children included by the parent doc. |
| `outputs/` | Every CSV/RDS/XLSX written by the pipeline, in subfolders numbered by pipeline step (`01_crc_harvest/`, `02_multi_fishery_creel/`, `03_district_creel/`, `04_interview_proportions/`, `05_assembly/`), plus `deliverables/` for the Jim status workbook and the simplified consultant-facing export. Nothing here is hand-edited; everything is regenerable by re-running the producing script. |
| `correspondence/` | Point-in-time status write-ups (e.g. `PST_FW_Effort_Status_Brief_2026-08-06.md`) — snapshots for external audiences, not living documentation. |

## Run order

Steps 1–4 are independent producers and can run in any order (or in
parallel); none of them runs at render time. Steps 5–7 run in sequence
afterward. `run_pst_pipeline.R` automates steps 1, 2, 3, 5, 6 — see that
script's header for exactly what it does and does not do.

| Step | Script | DB/VPN? | Writes to `outputs/` |
|---|---|---|---|
| 1 | `02_ingest/parse_crc_freshwater_harvest.R` | no | `01_crc_harvest/crc_freshwater_harvest_2010_2024_tidy.csv` |
| 2 | `02_ingest/multi_fishery_creel_summary.R` | **yes** | `02_multi_fishery_creel/multi_fishery_creel_{trips,harvest,qa,run_ledger,week_vs_month}.{csv,rds}` |
| 3 | `02_ingest/district_creel_ingestion.R` | no | `03_district_creel/district_creel_summary.csv` |
| 4 | `02_ingest/interview_proportions.qmd` | **yes** | `04_interview_proportions/interview_mode_location_props.csv`, `interview_batch_crosscheck.csv`, `all_interviews.{csv,rds}`, ~25 proportion/variability CSVs |
| 5 | `03_analysis/pst_fw_angler_trips_assembly.R` | no | reads 1–4 (from their subfolders above) plus `input_files/pst/lookup_tables/{pst_input_manifest,pst_river_block_crosswalk}.csv`, `input_files/pst/lookup_tables/crc_area_lut.csv`, and (if present) the NEPA workbook under `input_files/pst/external_data/`; sources `pst_p2_block_ratio.R` and `pst_crc_harvest_projection.R`; writes the `pst_fw_*.csv` family to `05_assembly/`, including the diagnostic `pst_fw_nepa_vs_pure_crc_comparison.csv` |
| 6 | `03_analysis/pst_fw_build_jim_workbook.R` | no | reads step 5's CSVs from `05_assembly/`; writes `deliverables/PST_FW_Status_Report.xlsx` (internal status) and `deliverables/PST_FW_Deliverable.xlsx` (simplified Year × River × Mode × Location × Angler Trips export for the consultant) |
| 7 | `quarto render pst_fw_angler_trips.qmd` | no | reads everything above; renders the parent doc |

## Parent/child Quarto structure

`pst_fw_angler_trips.qmd` is the **only** renderable document in this
subtree. Its children — everything under `01_intro_methods/` and the
`_20`/`_21`/`_22`-prefixed files under `03_analysis/` — are `{{< include
>}}` fragments, `_`-prefixed by convention, that share the parent's knitr
session and never render standalone.

`02_ingest/interview_proportions.qmd` is a **standalone producer**, not a
child of the parent doc. It is deliberately not included, because it needs
DB access and the parent is designed to render fully offline from committed
`outputs/` artifacts. Per design rule R2 (see below), the parent renders
cleanly from an empty or partial pipeline — every chunk that touches an
artifact checks for its presence first and reports a gap rather than
raising an error. Run `interview_proportions.qmd` manually, on its own,
whenever Track B proportions need refreshing.

## Design rule R2

Referenced throughout this subtree, defined in
`pst_fw_angler_trips_assembly.R`'s header:

> A missing input is logged as a gap, never silently dropped and never
> fatal. Partial assembly is the expected state until all providers return.

`run_pst_pipeline.R`, `pst_fw_angler_trips_assembly.R`, `pst_fw_build_jim_workbook.R`,
and `pst_fw_angler_trips.qmd` all follow this: a missing file produces a
logged gap (console message and/or a row in
`outputs/05_assembly/pst_fw_gap_register.csv`)
and the run continues with whatever is available, rather than stopping.
