# Unknown fin mark (UNK) reassignment

Optional pre-processing step in `template_scripts/fw_creel.Rmd` that assigns sampled fish with an unknown fin mark (UNK) to adipose-clipped (AD) or unmarked (UM) before effort and catch estimation.

## Files

| File | Purpose |
|---|---|
| `R_functions/reassign_unk_marks.R` | Reassigns UNK fish to AD/UM |
| `R_functions/summarise_unk_reassignment.R` | Reconciliation and bounds table |
| `R_functions/plot_unk_mark_reassignment.R` | Mark rate and sample size figures by time and section |
| `tests/test-reassign_unk_marks.R` | Tests (synthetic data only) |

## Usage

Set the report parameters in the YAML header of `fw_creel.Rmd`:

| Parameter | Default | Description |
|---|---|---|
| `unk_mark_reassign` | `"none"` | `"none"` or `"binomial"` |
| `unk_mark_species` | `""` | Species to reassign, e.g. `"Coho"`; `""` = all species |
| `unk_mark_min_known` | `10` | Interviews with known-mark fish a group needs before its mark rate is used |
| `unk_mark_window_weeks` | `1` | Weeks on either side pooled when a single week is too sparse |
| `unk_mark_kept_as_ad` | `FALSE` | `TRUE` for mark-selective fisheries: kept UNK fish are AD by regulation |
| `unk_mark_seed` | `1` | Random seed; change to produce an alternate reassignment |

Run the tests from the project root:

```r
testthat::test_file(here::here("tests", "test-reassign_unk_marks.R"))
```

## Methods

### Purpose

Some sampled fish are recorded with an unknown fin mark (UNK) because the adipose fin was not checked. Leaving them out would under-count marked (AD) and unmarked (UM) catch groups. Before estimating effort and catch, each UNK fish is assigned to AD or UM based on the mark rate of fish whose mark was recorded.

### Steps

1. **Select the fish.** Only fish recorded as UNK, for the species chosen in the report parameters, are reassigned. Fish with a known mark are not changed.
2. **Group similar fish.** A mark rate is always calculated within the same species and fate (kept or released), and where possible within the same life stage, section, and week.
3. **Require enough data.** A group's mark rate is used only if at least 10 interviews in that group recorded known-mark fish. Interviews are counted instead of fish because fish from the same angler group are not independent.
4. **Fall back when data are sparse.** If a group does not have enough data, the next broader group is tried, in this order:
   1. Same section and week
   2. Same week, all sections
   3. Same week plus the week before and after, all sections
   4. Whole season, same life stage
   5. Whole season, all life stages

   UNK fish that still do not have enough data stay UNK and are reported.
5. **Estimate the mark rate (beta step).** See [The beta-binomial approach](#the-beta-binomial-approach).
6. **Split the UNK fish (binomial step).** Each UNK record is split into UM and AD fish by a random draw using the group's mark rate. For example, a record of 5 UNK fish with a UM rate of 0.7 might become 4 UM and 1 AD.
7. **Optional rule for kept fish.** In mark-selective fisheries, kept UNK fish can be assigned to AD by regulation instead of by a mark rate.
8. **Check and report.**
   - **Totals:** the number of fish within each species, life stage, and fate must be unchanged; the code stops if it is not.
   - **Audit trail:** every reassigned record keeps its original mark (`fin_mark_raw`), the group used (`unk_stratum`, `unk_rate_level`), and the rate drawn (`unk_p_um`).
   - **Table:** shows how UNK fish were split, and the AD share if all UNK were UM or all were AD.
   - **Figures:** show observed and reassigned mark rates, with sample sizes, by week and section.

### The beta-binomial approach

The method estimates one mark rate per group with a **beta distribution**, then splits UNK fish with a **binomial** draw. It is not a regression: there are no predictors, only one rate per group.

**Beta step: how sure are we about the mark rate?**

- Count the known-mark fish in the group: $n_{UM}$ unmarked and $n_{AD}$ marked.
- The unmarked rate is described by a beta distribution:

$$
p_{UM} \sim \text{Beta}(n_{UM} + 0.5,\; n_{AD} + 0.5)
$$

- The "+0.5" on each side is a standard, minimally informative starting point (the Jeffreys prior). It keeps a small group, such as 10 AD and 0 UM, from implying a 0% UM rate with complete certainty.
- The distribution centres on the observed rate, and its spread shows how much data there is:

| Known-mark fish | Distribution | Average UM rate | Plausible range |
|---|---|---|---|
| 30 UM, 10 AD | Beta(30.5, 10.5) | about 0.74 | about 0.61–0.87 |
| 7 UM, 3 AD | Beta(7.5, 3.5) | about 0.68 | about 0.4–0.9 |

- One rate is drawn at random from this distribution for each group. Every UNK record in the group uses that same draw.

**Binomial step: how many UNK fish become UM?**

For a record of $k$ UNK fish:

$$
\text{UM fish} \sim \text{Binomial}(k,\; p_{UM}), \qquad \text{AD fish} = k - \text{UM fish}
$$

Counts stay whole numbers, so the reassigned data go straight into the existing PE and BSS estimation.

**Why this approach:** the beta distribution is the standard model for an uncertain proportion. It gives wider uncertainty to groups with less data, and it is simple to calculate. The random seed is fixed, so results can be reproduced.

### Assumptions

1. **UNK fish are like known fish in the same group.** Within a group, UNK fish have the same mark rate as fish whose mark was recorded. This matters most for released fish, which may go unchecked for reasons related to their mark.
2. **Mark rate differs by fate.** Kept and released fish are never pooled. In mark-selective fisheries they differ by regulation. In non-selective fisheries they may still differ (voluntary release, fish size, bag limits), and keeping them separate costs some precision but does not bias the results.
3. **Marks are recorded correctly.** Fish recorded as AD or UM are assumed correct.
4. **Mark rate is stable within the group used.** Where a sparse week falls back to neighbouring weeks or the whole season, the mark rate is assumed not to change much over that period.
5. **Sampled interviews represent the fishery** within each section and time period.
6. **Fish are treated as independent when estimating the rate.** The sample-size rule counts interviews, but the rate itself is based on fish counts, so precision may be slightly overstated when groups catch several fish.
7. **Single imputation.** UNK fish are reassigned once and then treated as observed, so effort and catch intervals do not include uncertainty in the mark rate. The bounds table shows how much the results could move.
8. **Scope.** Rates use all fetched data for the fishery, not only the estimation dates. Fish with a blank mark field are not treated as UNK and are not changed.

## Implementation notes

- **Run once.** Reassigning data that were already reassigned is an error. When working interactively, re-run the `dwg_fetch` and `manual_edits` chunks before re-running the reassignment chunk.
- **Catch group definitions change meaning.** A catch group defined with `fin_mark = "UM|UNK"` previously included all UNK fish; after reassignment it includes only the UNK fish assigned to UM, and AD groups gain fish.
- **Knitr cache.** Downstream chunks depend on `dwg_fetch` and the parameter hash only. After changing the reassignment code or manual edits with `enable_cache: TRUE`, clear the `.cache` folder.
- **Weeks start on Monday** and may not match the PE or BSS time strata.
- **Check fallback use** with `count(filter(unk_marks$catch, mark_imputed), unk_rate_level, wt = fish_count)`.
