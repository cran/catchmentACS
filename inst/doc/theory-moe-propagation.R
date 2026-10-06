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
# Drive-time areas and ACS data bundled with the package
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))

site_id    <- "AL_SITE_17"
drive_time <- 10L

iso_one <- iso[
  iso$site_id == site_id & iso$drive_time_min == drive_time, ,
  drop = FALSE
]

weighted <- cacs_intersect_weight(
  iso_sf           = iso_one,
  acs_sf           = acs,
  weight_method    = "area",
  keep_tract_audit = TRUE,
  verbose          = FALSE
)

propagated <- cacs_propagate_moe(weighted, verbose = FALSE)

## -----------------------------------------------------------------------------
VAR <- "B17001_002"

audit <- attr(weighted, "cacs_tract_audit") |>
  transmute(GEOID, w_cov = area_wt)

published <- acs |>
  sf::st_drop_geometry() |>
  filter(GEOID %in% audit$GEOID, variable == VAR) |>
  select(GEOID, M_j = moe)

hand_A <- audit |>
  left_join(published, by = "GEOID") |>
  arrange(desc(w_cov))

hand_A

## -----------------------------------------------------------------------------
z <- 1.645  # at another level, qnorm(1 - (1 - level) / 2)

se_term    <- hand_A$w_cov * hand_A$M_j / 1.645  # standard error of each term
hand_moe_A <- z * sqrt(sum(se_term^2))           # z * combined standard error

list(
  per_tract_var = round(se_term^2, 4),
  summed_var    = sum(se_term^2),
  hand_moe_A    = hand_moe_A
)

## -----------------------------------------------------------------------------
pkg_A <- propagated |>
  filter(variable == VAR) |>
  select(variable, estimate, moe, moe_formula_effective, moe_fallback)

pkg_A

all.equal(hand_moe_A, pkg_A$moe)

## -----------------------------------------------------------------------------
iso_08 <- iso[iso$site_id == "AL_SITE_08" & iso$drive_time_min == 10L, ,
              drop = FALSE]
propagated_08 <- cacs_propagate_moe(
  cacs_intersect_weight(iso_sf = iso_08, acs_sf = acs, verbose = FALSE),
  verbose = FALSE
)
rates_08 <- cacs_derive_rates(propagated_08, formula_dispatch = "auto",
                              verbose = FALSE)

## -----------------------------------------------------------------------------
rates_08 |>
  filter(variable %in% c("poverty_rate", "labor_force_participation")) |>
  select(variable, estimate, moe, moe_formula_requested,
         moe_formula_effective, moe_fallback, moe_fallback_reason) |>
  glimpse()

## -----------------------------------------------------------------------------
counts <- propagated_08 |>
  filter(variable %in% c("B17001_002", "B17001_001")) |>
  select(variable, estimate, moe)
counts

A   <- counts$estimate[counts$variable == "B17001_002"]  # numerator
B   <- counts$estimate[counts$variable == "B17001_001"]  # denominator
M_A <- counts$moe[counts$variable == "B17001_002"]
M_B <- counts$moe[counts$variable == "B17001_001"]
R   <- A / B

list(
  relative_moe          = c(numerator = M_A / A, denominator = M_B / B),
  under_root_proportion = M_A^2 - R^2 * M_B^2,
  ratio_formula         = sqrt(M_A^2 + R^2 * M_B^2) / B,
  package               = rates_08$moe[rates_08$variable == "poverty_rate"]
)

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

