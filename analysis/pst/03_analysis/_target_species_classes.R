# ==============================================================================
# _target_species_classes.R
#
# Shared classification of the creel interview field `target_species`, sourced
# by creel_guided_species_seasonality.R and guided_target_mix_by_river.R so the
# two cannot drift apart on what counts as a salmon trip.
#
# The field is not a clean binary. Several values are themselves ambiguous about
# salmon vs steelhead - the exact distinction that decides whether a trip belongs
# against a salmon-only denominator - and one records that the question was
# never asked, which must count as unanswered or coverage is overstated.
# ==============================================================================

TARGET_CLASS <- c(
  Chinook = "salmon", Chum = "salmon", Coho = "salmon",
  Pink = "salmon", Sockeye = "salmon",
  `Multiple salmon and/or steelhead targeted` = "salmon_or_steelhead",
  Salmonid = "salmon_or_steelhead",
  Steelhead = "steelhead",
  `Bull Trout` = "other_species", Cutthroat = "other_species",
  `Rainbow Trout` = "other_species", Trout = "other_species",
  Whitefish = "other_species", Sturgeon = "other_species",
  Bass = "other_species", Carp = "other_species", Crappie = "other_species",
  `Yellow Perch` = "other_species",
  `Any species` = "nonspecific", Other = "nonspecific", Unknown = "nonspecific",
  `Target species not asked` = "not_asked"
)

# Classes that represent an actual answer. blank / not_asked are excluded from
# every share and every coverage figure.
ANSWERED_CLASSES <- c("salmon", "salmon_or_steelhead", "steelhead",
                      "other_species", "nonspecific", "unmapped")

classify_target <- function(x) {
  x <- stringr::str_squish(x)
  out <- unname(TARGET_CLASS[x])
  out[is.na(out) & !is.na(x) & nzchar(x)] <- "unmapped"
  out[is.na(x) | !nzchar(dplyr::coalesce(x, ""))] <- "blank"
  out
}
