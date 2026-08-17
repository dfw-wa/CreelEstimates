# PST Economic Analysis — Freshwater Recreational Effort
## Status Brief

**To:** Jim Scott
**From:** Evan Booher, Freshwater Fisheries Policy Coordinator
**Date:** August 6, 2026
**Re:** Data compiled to date, method update from NMFS, and decisions needed

---

## Bottom line

Three of the four regional blocks now have a defensible path. The Columbia
mainstem is essentially complete and already in the consultant's requested
format. Puget Sound has a method, but the underlying trips-per-fish rates
changed this week and I recommend we adopt NMFS's updated values. The
**2025 remains the one true gap.** Final CRC harvest is in hand for
2022–2024; 2025 does not publish until October, which constrains any
system where we extrapolate from CRC. See §5.

---

## 1. What is compiled and in hand

| Region / source | Years | What it provides | Status |
|---|---|---|---|
| **Lower Columbia (below Bonneville)** — OR/WA Joint Staff Reports, via Mark S. | 2022–2025 | Salmonid angler trips split **Bank / Private Boat / Guided Boat**, plus Chinook kept/released | **Complete.** 173,122 (2022) → 195,859 (2023) → 230,050 (2024) → 223,062 (2025) |
| **Buoy 10 estuary** — JSR tables | 2022–2025 | Same mode/location split | Complete; scope to be confirmed with the consultant. 85,187 / 78,179 / 99,100 / 104,110 |
| **Bonneville–McNary** — JSR tables | 2022–2025 | Total salmonid angler trips + Chinook/coho catch; **no mode or bank/boat split** | Totals in hand: 31,252 / 58,271 / 51,669 / 42,811. Mode split not available |
| **WDFW creel — effort** (18 river fisheries) | 2022–2025 | Direct effort estimates by month, bank/boat, from `CreelEstimates` | ~6.2M angler-hours; ~1.14M expanded angler trips. See §3 |
| **WDFW creel — harvest** (new this week) | 2022–2025 | Creel-derived salmon harvest by species, month, bank/boat, with variance and CVs | 49 fishery-years estimated; 3 errors, 1 skipped. See §3 |
| **Puget Sound NEPA effort workbooks** — via Christina Iverson | Historic **2005–2024** + 2026/27 projections | CRC harvest × trips-per-fish ratio, ~54 PS systems, river × month, **totals only** | 2022–2024 extracted and joinable to creel. Rates being updated (§2) |
| **Final CRC harvest workbooks** — via Heidi, Harvest Data Team | 2021–2024 | Freshwater salmon harvest by system, the denominator for all ratio-based estimates | **2022–2024 final, in hand.** 2025 does not publish until October — see §5 |
| **NMFS PS RMP DEIS inputs + 2013 memo** — via James Dixon | — | The origin and current version of the trips-per-fish method | Received Aug 5 (§2) |
| **Mitchell Act FEIS Appendix J** — via James Dixon | — | Confirms FEIS catch/trip rates are uniformly applied constants, not regional estimates | Reviewed; not usable as regional rates |

---

## 2. Method update from NMFS (new this week)

James Dixon located the 2013 NOAA memo Christina and I had been seeking
since 2024, **and** provided the values NMFS is currently using in the
Puget Sound RMP DEIS. The 2013 rates have been superseded:

| | 2013 memo (in our NEPA workbook) | NMFS 2024 update | Anchor year |
|---|---|---|---|
| Odd-year salmon | 3.44 trips/fish | **3.54** | 2011 |
| Even-year salmon | 8.65 trips/fish | **4.82** | **2022** (was 2006) |

Two things follow:

**The even-year discrepancy is smaller than I reported, and the fix is
different.** Our NEPA workbook applies the odd-year rate to even years. I
previously flagged this as a ~60% understatement correctable by
reinstating 8.65. Against NMFS's current values, the workbook understates
even-year effort by **~29%**, and reinstating 8.65 would **overstate by
~80%**. I am no longer recommending 8.65.

**Recommendation:** use the NMFS 2024 rates for the Puget Sound block.
They are the originating agency's current values, they are in active use
in a concurrent NMFS EIS, and the even-year rate is anchored on 2022 —
inside the consultant's window — rather than 2006. Diverging from NMFS
here would be hard to defend.

**Two further items from the memo worth your awareness:**

- **Units.** The memo documents that the source survey yields angler
  **days**, converted to trips at 0.961 trips/day (2011), then converted
  back to days for the economic valuation. The consultant's "Angler Days"
  is the *native* unit of this lineage, and a documented conversion exists.
  This resolves a concern I had flagged earlier.
- **Species.** The memo treats "salmon" as a single combined category by
  explicit assumption. This supports reporting Puget Sound combined rather
  than split by Chinook/coho — splitting would double-count anglers who
  landed both.

I have asked James for the underlying derivation workbook and the WDFW
2024 effort source, which he referenced but did not attach.

---

## 3. Creel work — `CreelEstimates`, branch `chore/multi-fishery-trip-summary`

<https://github.com/dfw-wa/CreelEstimates/tree/chore/multi-fishery-trip-summary>

This branch produces a standardized multi-fishery trip summary from our
creel program — total effort hours, completed-trip interviews, mean trip
length and party size, and an expanded trip estimate — by fishery, year,
month, catch area, and bank/boat. It is what lets us use **observed**
effort instead of a ratio wherever we have surveys.

**Coverage:** 18 fisheries, 2022–2025.

- *Puget Sound:* Skagit, Snohomish, Stillaguamish, Cascade, Nooksack,
  Wallace, Puyallup/Carbon, Nisqually
- *WA Coast:* Chehalis (incl. Upper/Lower), Humptulips, Satsop,
  Quillayute, Hoh
- *Columbia tributaries:* Drano Lake, Lower Cowlitz

**New this week: creel-derived harvest estimates.** The branch now also
produces salmon harvest estimates by species, month and bank/boat, with
variance and confidence intervals, plus a QA table and a run ledger. This
matters for two reasons. First, it lets us compute a trips-per-fish ratio
**entirely within creel** — both numerator and denominator from the same
survey — which avoids the CRC provenance problem noted in §6. Second, it
gives us the first independent check on the NMFS rates:

| Region (surveyed fisheries, 2022–2025) | Even years | Odd years | Even : odd |
|---|---|---|---|
| Puget Sound | 3.63 | 2.04 | **1.78×** |
| WA Coast | 2.07 | 2.29 | **1.11×, inverted** |
| *NMFS 2024 rates, Puget Sound* | *4.82* | *3.54* | *1.36×* |

The odd/even pattern is **independently confirmed in Puget Sound** and is
somewhat stronger in our own data than in the NMFS rates. It is **absent
on the coast**, which is direct empirical support for not extending the
Puget Sound structure westward. These creel values are not a replacement
for the NMFS rates — creel covers surveyed reaches and mostly fall
seasons, while the NMFS/CRC ratio is annual and basin-wide — but they are
the right basis for coastal ratios, where creel is our primary source.

**Three caveats.** Coverage is uneven year to year. Four fisheries
(Puyallup/Carbon, Nisqually, Drano, Lower Cowlitz) have effort hours but
no completed trip expansion — Puyallup/Carbon is the material one, with
roughly 103,000 salmon harvested in 2023 and 96,000 in 2025 and no trip
estimate attached. And Lower Cowlitz is currently failing to estimate on a
data-formatting issue; it is our only Lower Columbia tributary creel.

The guided-vs-private split is not in this summary; it comes from creel
interview records and is being derived separately.

---

## 4. Method by region — where we landed

- **Puget Sound:** CRC harvest × NMFS 2024 rates, with direct creel
  substituted where surveys exist.
- **Columbia mainstem:** observed JSR estimates. No ratio needed.
- **WA Coast:** direct creel for the major systems. For unsurveyed rivers,
  a **coastal** ratio derived from our own paired creel/CRC data.
- **Upper Columbia and tributaries:** no creel coverage. Pursuing empirical
  effort/catch from the Columbia River Division; response pending from Mark.

I looked closely at whether to extend the NMFS Puget Sound approach to the
coast and upper Columbia and concluded **no**. That method allocates
statewide survey effort to a region using the region's share of statewide
landed catch — an assumption that breaks down where release rates are high
and anglers target multiple species, which describes the coast, and it
carries a pink-cycle odd/even structure that has no meaning above
Bonneville. We have better data in both places.

---

## 5. Open question — 2025 CRC harvest

**Final CRC harvest for 2022–2024 is in hand**, so those three years are on
solid footing across all regions. **2025 CRC will not publish until
October.** Anywhere we rely on CRC harvest × a trips-per-fish ratio, we
have no denominator for 2025 until then — which is after the consultant's
timeline as I understand it.

We have partial coverage from other directions: the Columbia mainstem is
observed data from the Joint Staff Reports and does not depend on CRC at
all; Puget Sound can be carried on NEPA projections if clearly labeled as
projections; and creel gives us 2025 directly for Skagit, Snohomish,
Stillaguamish, Chehalis, Humptulips, Satsop, Quillayute and Hoh. The gap is
the **unsurveyed systems in 2025**, mainly on the coast and in smaller
Puget Sound rivers, where extrapolation from CRC harvest is the only method
available.

Options are to deliver 2022–2024 complete and flag 2025 as partial, to
deliver 2025 for surveyed systems only, or to hold the affected block until
October. This is ultimately a schedule question for the consultant, and I'd
rather surface it now than in September. Next week I'll size it — which
systems actually depend on CRC for 2025, and how much effort they
represent.

## 6. Next week

- Call with Christina (Monday) on the NEPA workbook odd/even application —
  now a question of internal consistency for the BIA package, separate
  from our deliverable
- Follow up with James Dixon for the derivation workbook and effort source
- Follow up with Mark and CRD on upper Columbia empirical effort/catch,
  and on the Buoy 10 / lower Columbia overlap
- Request a provenance flag from the Harvest Data Team distinguishing
  punch-card-expanded from creel-substituted CRC harvest, which affects any
  ratio we derive
- Size the 2025 extrapolation gap: which systems depend on CRC for 2025
  and how much effort they represent
- Confirm remaining scope items with the consultant (Buoy 10, upriver
  spring Chinook, PSC indicator-stock sideboards)
- Complete the trip expansion for the four creel fisheries currently
  missing it, and clear the Lower Cowlitz estimation errors
- Validate the automated river-name match between the NEPA CRC estimates
  and the creel fisheries
