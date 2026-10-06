## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  message = FALSE,
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  out.width = "85%"
)

## -----------------------------------------------------------------------------
library(catchmentACS)
library(dplyr)
library(sf)  # needed to subset the bundled sf objects with [

## -----------------------------------------------------------------------------
# Compute every result in this article instead of reading saved ones; the
# option is restored at the end of the article.
old_options <- options(catchmentACS.cache_enabled = FALSE)

## -----------------------------------------------------------------------------
# ACS codes of the numerator (num) and the denominator (den) of each rate
cacs_acs_default_rates

## -----------------------------------------------------------------------------
# Drive-time areas and made-up ACS data bundled with the package
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))

site_id    <- "AL_SITE_03"
drive_time <- 15L

iso_one <- iso[
  iso$site_id == site_id & iso$drive_time_min == drive_time, ,
  drop = FALSE
]

weighted <- cacs_intersect_weight(
  iso_sf           = iso_one,
  acs_sf           = acs,
  weight_method    = "area",
  verbose          = FALSE
)
propagated <- cacs_propagate_moe(weighted, verbose = FALSE)

# "auto": the proportion formula for poverty_rate and
# labor_force_participation, the ratio formula for the other three rates
rates <- suppressWarnings(
  cacs_derive_rates(propagated, formula_dispatch = "auto", verbose = FALSE)
)

## -----------------------------------------------------------------------------
z      <- 1.645
totals <- attr(weighted, "cacs_aggregation_carriers")

# The weighted count (est_total) or its variance (var_total_raw) for one ACS code
total_of <- function(code, col) totals[[col]][totals$variable == code]

totals |>
  filter(variable %in% c(
    "B22003_002", "B22003_001",   # snap_rate  num / den
    "B23025_002", "B23025_001",   # labor_force_participation num / den
    "B17001_002", "B17001_001"    # poverty_rate num / den
  )) |>
  select(variable, est_total, var_total_raw, weight_sum, n_tracts)

## -----------------------------------------------------------------------------
A_snap    <- total_of("B22003_002", "est_total")
B_snap    <- total_of("B22003_001", "est_total")
VarA_snap <- total_of("B22003_002", "var_total_raw")
VarB_snap <- total_of("B22003_001", "var_total_raw")

R_snap        <- A_snap / B_snap                                     # the rate A / B
hand_moe_snap <- z * sqrt(VarA_snap + R_snap^2 * VarB_snap) / abs(B_snap)

pkg_snap <- rates |>
  filter(variable == "snap_rate") |>
  select(estimate, moe, moe_formula_effective, moe_fallback)

list(
  A = A_snap, B = B_snap,
  hand_estimate = R_snap,
  hand_moe_C2   = hand_moe_snap,
  pkg           = pkg_snap
)

## -----------------------------------------------------------------------------
all.equal(
  c(estimate = R_snap, moe = hand_moe_snap),
  c(estimate = pkg_snap$estimate, moe = pkg_snap$moe)
)

## -----------------------------------------------------------------------------
A_lfp    <- total_of("B23025_002", "est_total")
B_lfp    <- total_of("B23025_001", "est_total")
VarA_lfp <- total_of("B23025_002", "var_total_raw")
VarB_lfp <- total_of("B23025_001", "var_total_raw")

R_lfp       <- A_lfp / B_lfp
under_root  <- VarA_lfp - R_lfp^2 * VarB_lfp           # under the proportion formula's root
hand_moe_C1 <- z * sqrt(under_root) / B_lfp
hand_moe_C2 <- z * sqrt(VarA_lfp + R_lfp^2 * VarB_lfp) / abs(B_lfp)  # for comparison

list(
  under_root  = under_root,     # positive, so the proportion formula can be used
  hand_moe_C1 = hand_moe_C1,
  hand_moe_C2 = hand_moe_C2     # the ratio formula
)

## -----------------------------------------------------------------------------
pkg_lfp <- rates |>
  filter(variable == "labor_force_participation") |>
  select(estimate, moe, moe_formula_effective, moe_fallback, moe_fallback_reason)

pkg_lfp

all.equal(hand_moe_C1, pkg_lfp$moe)

## -----------------------------------------------------------------------------
rates |>
  filter(estimand_family == "derived_rate") |>
  select(variable, estimate, moe,
         moe_formula_requested, moe_formula_effective,
         moe_fallback, moe_fallback_reason,
         n_tracts, n_tracts_num, n_tracts_den) |>
  print(width = Inf)

## -----------------------------------------------------------------------------
acs_drop <- acs[sf::st_drop_geometry(acs)$variable != "B19056_002", ]

weighted_d   <- cacs_intersect_weight(
  iso_sf = iso_one, acs_sf = acs_drop,
  weight_method = "area", verbose = FALSE
)
propagated_d <- cacs_propagate_moe(weighted_d, verbose = FALSE)
rates_d      <- suppressWarnings(cacs_derive_rates(propagated_d, verbose = FALSE))

rates_d |>
  filter(estimand_family == "derived_rate") |>
  select(variable, estimate, moe,
         moe_fallback_reason, failure_origin, n_tracts_num, n_tracts_den) |>
  print(width = Inf)

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

