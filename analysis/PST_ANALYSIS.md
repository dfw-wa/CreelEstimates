# PST Freshwater Recreational Effort — analysis contract

**Branch:** `dfw-wa/CreelEstimates` @ `chore/multi-fishery-trip-summary`
**Deliverable:** Year × River × Mode (guided/unguided) × Location (bank/boat) × Angler Trips, 2022–2025
**Recipient:** Melissa Errend, Northern Economics · **Scope authority:** Jim Scott
**Scope:** PST salmon, total (not species-specific). Steelhead excluded. Columbia block is OR+WA combined; all other blocks WA-only.

**Delivery scope note:** Buoy 10, Lower Columbia, and Bonneville–McNary (Columbia mainstem) are **excluded from delivery**. Those ODFW files were transmitted to WDFW *by Northern Economics* — the consultant already has this data, so we do not send it back. `pst_fw_effort_assembly.R` never ingests this block; `DELIVER_BLOCKS` gates the deliverable to Puget Sound, WA Coast, and Columbia tributaries only. The block stays in the crosswalk purely for documentation, in case it's ever needed for internal QC.

**Units note:** the consultant's request is phrased in "angler days"; everything this pipeline produces is angler **trips** (`total_trips_est = total_effort_hrs / mean_trip_length`). Trips and days are not interchangeable — some fisheries run multi-day trips, some anglers take more than one trip per day — and no trips→days conversion happens anywhere in this repo. That conversion, if it's needed, is a decision for Jim/Melissa, not a pipeline assumption. Every script, column, and output file in this repo says `trips`, not `days`.

---

## 1. Run order

| # | Script | Writes |
|---|---|---|
| 1 | `analysis/parse_crc_freshwater_harvest.R` | `outputs/crc_freshwater_harvest_2021_2024_tidy.csv` |
| 2 | `analysis/multi_fishery_creel_summary.R` | `outputs/multi_fishery_creel_trips.csv` + harvest/QA/ledger |
| 3 | `analysis/mid_columbia_yakima_creel_ingestion.R` | `outputs/mid_columbia_yakima_creel_summary.csv` |
| 4 | `analysis/interview_proportions.qmd` | `outputs/interview_mode_location_props.csv` |
| 5 | **`analysis/pst_fw_effort_assembly.R`** | **the deliverable + three ledgers** |

Steps 1–4 are independent and parallelizable. Step 5 is the only writer of the deliverable, and it is re-runnable at any time — it reads whatever is present and logs whatever isn't.

## 2. Repo moves needed

These are housekeeping, but they are why the inputs currently feel scattered:

| Move | From | To | Why |
|---|---|---|---|
| `interview_proportions.qmd` | `input_files/` | `analysis/` | It is code, not an input. Track B engine. |
| `multi_fishery_trip_summary.csv`, `results.csv` | repo root & `analysis/outputs/` | `outputs/archive/` or delete | **Superseded** by `multi_fishery_creel_trips.csv`. Leaving them on disk already caused one analysis to be run against stale numbers. |
| ODFW / JSR / NEPA / NMFS files | Claude project only | `input_files/pst/external/` | **Half the inputs are not in the repo.** Nothing is reproducible from a clean clone until they are. |
| `crc_areas_creeled.csv` | `evanbooher/crc_mapping` | `input_files/pst/` | Coverage status must be one table, not two. |

Add `input_files/pst/external/` to `.gitignore` only if file size forces it — otherwise vendor them, they are small.

## 3. Input registry

`input_files/pst/pst_input_manifest.csv` — one row per input, machine-readable, with `tier`, `period_basis`, `unit_declared`, `status`, and `blocking`. The assembly script reads it and refuses to run silently past anything flagged `blocking`.

`input_files/pst/pst_river_block_crosswalk.csv` — 63 rows, generated from the branch's own data, resolving every creel fishery-year and provider river to a block, a delivery river label, and its CRC areas. Every fishery resolved cleanly; there are no `REVIEW` rows.

| Block | Rows | Primary source | Delivered? |
|---|---|---|---|
| Puget Sound | 24 | creel PE | Yes |
| WA Coast | 19 | creel PE | Yes |
| Columbia tributaries | 17 | `R3_external` (Hanford, Yakima, McNary) — creel PE nominally covers Drano but currently returns zero usable effort | Yes |
| Columbia mainstem | 3 | ODFW (Buoy 10, LCR, Bonneville–McNary) | **No — consultant-supplied, out of scope** |

## 4. Output contract

| File | Grain | Purpose |
|---|---|---|
| `pst_fw_trips_by_mode_location_INTERMEDIATE.csv` | block × river × year × mode × location | **Intermediate — not the deliverable.** Trips + paired total-salmon harvest + `trips_per_salmon`. See §8. |
| `pst_fw_trips_by_crc_area_INTERMEDIATE.csv` | block × river × **CRC area** × year × mode × location | Same as above but retains `catch_area_code` — **use this one for CRC joins** |
| `pst_fw_p2_block_ratios.csv` | block × year | Paired trips-per-salmon derived from the merged creel run |
| `pst_fw_crc_vs_creel_bias.csv` | CRC area × year | CRC vs design-based creel harvest, same area-year. See §8.6 |
| `pst_fw_effort_long.csv` | + month, fishery_name | Audit trail / re-aggregation |
| `pst_fw_provenance_ledger.csv` | river-year × tier × source | Which number came from where |
| `pst_fw_gap_register.csv` | source × severity | Every blocker, gap, defect, and decision |
| `pst_fw_crc_coverage.csv` | CRC area | Feeds the status map |

Every deliverable row carries `tier` and `source_id`. There are no unattributed numbers.

## 5. Design rules encoded in the script

- **R1** Every row carries tier + source.
- **R2** A missing input logs a gap; it is never fatal and never silent. Partial assembly is the expected state.
- **R3** A missing dimension is `"unknown"`, never zero. Hanford Reach has no guided/unguided field collected — that is a structural gap, not a stratum estimated at zero.
- **R4** Ratios never cross blocks — not even within one river. JSR-verified trips-per-salmon, Columbia mainstem 2022–2025 (Spring JSR Table 24; Fall JSR Tables 25–26): Buoy 10 1.4–2.8, LCR fall (Aug–Oct) 2.75–3.95, LCR spring (Feb–Jun 15) 7.0–13.0, LCR summer (Jun 16–Jul) 9.8–**60.7**. The 60.7 is real, not a typo — Jun 16–30 2025 (9,169 trips) and Jul 2024 (15,704 trips) both had zero kept Chinook, consistent with the weak/restricted lower Columbia summer Chinook returns those years. Hanford Reach (Columbia tributaries) ≈ 2.48. Puget Sound (NMFS 2024 rates) 3.5–4.8. There is no defensible single rate at any scale broader than one fishery-month.
- **R5** Projections and donor-borrowed values are labeled in `method` and never collapse into an unlabeled total.

Tier hierarchy: **P1** design-based creel → **P2** within-block creel-to-CRC ratio → **P3** published NEPA/FEIS fallback.

## 6. Open blockers

| # | Item | Owner | Effect if unresolved |
|---|---|---|---|
| 1 | Guided/unguided absent for all mid-Columbia | Todd Miller | Whole block delivers `mode = "unknown"` |
| 2 | 2025 coastal freshwater has no fallback identified | — | 2025 coastal is empty; PS covered by NEPA projection |
| 3 | Upper Columbia (Chad Jackson) / Snake (Jeremy Trump) | pending since 8/11 | Thinnest block; no P1 at all |

Resolved by scope cut: the LCR undocumented scalar, the Bonneville–McNary totals-only split, and the Buoy 10 Charter Boat mode question are no longer delivery blockers — that whole block is out of scope (§ above). They'd only resurface if Jim/Melissa ever want WDFW to independently reproduce or reconcile against the consultant's own ODFW numbers.

## 7. Known defects

- **`mid_columbia_yakima_creel_ingestion.R`** — `Hanford Reach fall Chinook 2025` rows carry `year = 2024`. The assembly script works around it by trusting the year token in `fishery_name` and logging every corrected row, but the ingestion script should be patched.
- **`multi_fishery_creel_run_ledger.csv`** — 98 rows, all `ok` as of the 2026-08-14 run. The previous 3 errors / 1 skip are resolved. Lower Cowlitz remains absent from the output entirely: it lacks the data needed for effort estimation, a genuine gap rather than a pipeline bug.
- **Superseded files on disk** — `multi_fishery_trip_summary.csv` and `results.csv` are stale but still present. They already caused one analysis to be run against wrong numbers. The assembly script now logs a `defect` if it finds them; move to `outputs/archive/` or delete.
- **CRC compile** — region labels lost for 2023–2024; subtotal blocks retained as leaf rows causing ~2× double-count. Confirm the current `crc_freshwater_harvest_2021_2024_tidy.csv` postdates the fix.
- **NEPA workbook** — fill-right errors in the 2022/2024 even-year columns.

## 8. Why the assembly output is not the deliverable

`pst_fw_effort_assembly.R` currently answers "what can we support right now," not "what do we owe Northern Economics." Five things stand between the two.

### 8.1 Trip expansion — RESOLVED

An earlier version of this document reported that 29% of measured effort was being silently dropped (Nisqually, Puyallup/Carbon). **That finding was an artifact of reading a superseded file** (`multi_fishery_trip_summary.csv`). Against the current `multi_fishery_creel_trips.csv`:

- All 49 fisheries expand cleanly — **zero** with `NA` trips.
- The run ledger is 98 rows, **all `ok`** — no errors, no skips.
- Nisqually 2022 *and* 2023, all four Puyallup/Carbon years, and all four Drano years now carry trip estimates. The `[S4]` donor hierarchy is doing the work: 778 rows use own-month trip length, 166 `area_season`, 66 `type_season`, 8 `fishery_season`.
- Lower Cowlitz is absent entirely — the one genuine, confirmed data gap.

The assembly script still logs expansion failures if they reappear, but there are none today.

**The real hazard in the new file is the opposite of a drop — it's a double-count.** `multi_fishery_creel_trips.csv` stacks both the week- and month-stratified runs, so every stratum appears twice. Naively summing gives 1,881,197 trips against a correct month-basis figure of 954,177. The assembly script filters to `PE_PERIOD = "month"` and asserts uniqueness of the design grain afterward.

### 8.2 P2 ratios now derived, expansion still not wired in

`build_block_ratios()` previously computed only a CRC harvest denominator and never built a ratio at all. It now derives **observed trips-per-salmon from the paired merged creel outputs** — both numerator and denominator from the same `est_pe_effort()` / `est_pe_catch()` run on the same strata, so the ratio is internally consistent rather than two independent estimates divided:

| Block | 2022 | 2023 | 2024 | 2025 |
|---|---|---|---|---|
| Puget Sound | 4.24 | 1.45 | 3.15 | 1.35 |
| WA Coast | 1.98 | 2.53 | 1.93 | 2.19 |
| Columbia tribs | 2.76 | 1.89 | 2.11 | 1.71 |

Puget Sound odd years sit far below even years — the pink-salmon signal the NEPA workbook encodes as its 3.44/8.65 odd/even split. Direction matches independently, which is a useful check that the pairing is right. Magnitudes differ from NEPA's because these are creel-observed totals rather than CRC-expanded.

Ratios are computed as a **ratio of sums** within block-year, not a mean of per-stratum ratios — strata with near-zero harvest produce enormous unstable ratios that a mean would let dominate.

**What's still missing:** applying those ratios to uncovered river-years. That needs a validated CRC-stream-to-river crosswalk, which doesn't exist; inventing one silently would be worse than the gap. Logged as a `blocker`. Given 127 CRC areas carry harvest with no creel behind them, this remains a large share of the eventual deliverable.

### 8.3 P3 fallbacks not ingested

`ingest_published_fallback()` only logs notes. The NEPA projection for Puget Sound 2025 and any coastal 2025 fallback are not read.

### 8.4 `mode = "unknown"` is unresolved, not an answer

All `R3_external` rivers, plus any fishery-year below `MIN_INTERVIEWS`, carry `mode = "unknown"`. The console prints `pct_mode_unknown` per block for exactly this reason. A deliverable with an unexplained "unknown" mode category needs either a documented donor decision from Jim or an explicit unclassified category agreed with Melissa.

### 8.5 Trips vs. angler-days still open

See the units note at the top. No conversion is applied anywhere; that decision hasn't been made.

### 8.6 CRC vs creel bias — now measurable

`catch_area_code` is carried through the whole pipeline rather than collapsed at the river roll-up, because it is the only key that joins creel effort back to CRC harvest. Rivers can span several CRC areas (Quillayute is 398|400|402|404|406), so rolling to river grain destroys that key one-way. Two roll-ups are written: river grain for the consultant's requested shape, area grain for this comparison.

That makes the bias question empirical. Both sides estimate total salmon kept in a CRC area-year by completely different means — creel is a design-based on-the-ground estimate, CRC is an angler-reported card expansion:

- **74 CRC area-years pair with creel** across 2022–2025.
- **Median CRC/creel harvest ratio ≈ 0.60** — CRC reports roughly 40% less harvest than the design-based estimate for the same area-year.
- Direction is consistent with expected card under-reporting, and it is not uniform: area 618 (Drano) runs *above* 1.0 in 2023–2024 while area 358 (Humptulips) sits near 0.35. Another instance of [R4] — the bias doesn't travel between systems any better than the ratios do.

Two caveats encoded in the output rather than left to the reader:

- **Basis mismatch is real, not a bug.** CRC is license-year (Apr 1 – Mar 31); creel is calendar-month. The comparison is indicative at annual grain, not a month-for-month reconciliation.
- **2025 pairs return zero CRC harvest** because CRC publishes through license year 2024 only. Those rows join but are flagged `comparable = FALSE` and excluded from the median, so a coverage artifact isn't read as a reporting collapse.

This matters for P2 directly: if CRC is the harvest denominator for uncovered river-years but runs ~0.6× the design-based truth, then expanding CRC harvest by a creel-derived trips-per-salmon ratio inherits that bias. Whether to correct for it is a decision for Jim, not something to bury in the pipeline.
