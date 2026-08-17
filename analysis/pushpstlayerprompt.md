Run this in a Claude Code session with a local checkout of dfw-wa/CreelEstimates
on branch chore/multi-fishery-trip-summary (the branch already has, as of
commit a83b7fa: the merged multi_fishery_creel_summary.R pipeline, the trip-
expansion donor-hierarchy fix, the Lower Cowlitz KNOWN_FAILED exclusion, and
the Hamma Hamma/Cedar River LUT-gap exclusion in prep_crc_trip_estimates.R).

---

PROMPT:

I need to get the PST assembly/framework layer of this project onto the
remote branch chore/multi-fishery-trip-summary. It's almost certainly sitting
locally and never got pushed, because .gitignore excludes /project_scripts
and /project_outputs wholesale, and these files likely live there.

Find and report the status (present locally / already tracked / untracked-
ignored / not found) of each of the following, then push whatever is present
but missing from the remote:

Core framework/pipeline (expected likely in project_scripts/ or repo root):
  - PST_FW_Effort.qmd          -- parent framework doc: scope, two-track
                                   decomposition (Track A trip totals via
                                   P1/P2/P3 hierarchy, Track B mode/location
                                   proportions), assumptions A1-A4, design
                                   rules R1-R5, delivery plan
  - pst_fw_effort_assembly.R   -- single orchestrator script; exports
                                   PST_FW_Jim_Update.xlsx with seven tabs
  - pst_p2_block_ratio.R       -- P2 CRC-ratio expansion module, keyed on
                                   catch_area_code, leave-one-out validation,
                                   Hanford Reach 534/535/536 double-count guard
  - patch_crosswalk_areas.R    -- adds CRC codes for Hanford (534/535/536),
                                   McNary (533), Yakima (690); drops
                                   out-of-scope ColumbiaMainstem rows
  - parse_crc_harvest.py       -- parses Heidi's CRC harvest Excel workbooks
                                   (has fixes for 2023/2024 region-label loss
                                   and subtotal double-counting)

QA/reporting artifacts (expected likely in project_outputs/):
  - crc_status_interactive.html -- Leaflet QA map: year switching,
                                    status/coverage toggle, region filter,
                                    per-CRC popups
  - PST_FW_Jim_Update.xlsx      -- the assembly script's output workbook
                                    (only push if you want this checked in as
                                    a snapshot -- it's a generated artifact,
                                    so treat as optional / discuss)

Reference input files mentioned as in-project but NOT found on the remote
branch (verified against input_files/ on origin/chore/multi-fishery-trip-summary
as of a83b7fa -- do not re-list anything already there, the remote already has
the Yakima/McNary 2023-2025 workbooks, both JSR fall docs, LCR and Buoy 10
trip files, and Salmon Freshwater Estimates 2022-2024):
  - 2026-or-wa-spring-joint-staff-report.pdf   (only the FALL staff report
                                                 PDF is on the remote; spring
                                                 JSR only exists as .xlsx)
  - NEPA PS_Recreational Effort Estimates 2025_2026_4_18_2025.xlsx
                                                 (an EARLIER NEPA workbook
                                                 version than the one already
                                                 committed -- confirm this is
                                                 actually a distinct file you
                                                 still need, not a duplicate)
  - NEPA_Puget_Sound_Recreational_Salmon_Angling_Trips_2023_2024_4_16_CI_CJ042123_2024_Working_Changes.xlsx
  - Salmon Freshwater Estimates 2024 Final.xlsx (remote only has the
                                                  non-"Final" 2024 version --
                                                  confirm which is authoritative)
  - PSMFC_RecFin_Data_Template.xlsx             (consultant delivery template)
  - ODFW_recreational_data_template.xlsx        (consultant delivery template)
  - email.pdf through email13.pdf               (correspondence trail -- confirm
                                                  before pushing: emails may
                                                  contain PII/sensitive content
                                                  not appropriate for a repo
                                                  that could be shared with
                                                  the consultant or others)

McNary 2022 workbook: confirmed genuinely missing per Todd Miller (not a
push-it-from-local situation) -- no action needed here, just don't treat its
absence as a bug.

For each file found locally: `git add -f` it (needed because of .gitignore),
commit with a message noting it's the PST assembly/framework layer catching
up to the branch, and push to origin chore/multi-fishery-trip-summary. If
project_scripts/ or project_outputs/ turn out to hold a lot of scratch/
intermediate cruft alongside the real deliverables, don't force-add the whole
directory -- cherry-pick the specific files above.

Flag anything ambiguous (e.g. multiple candidate files, generated-vs-source
artifacts, or anything that looks like it might contain PII) rather than
guessing.
