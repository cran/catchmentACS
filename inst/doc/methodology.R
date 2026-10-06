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
# The drive-time areas and ACS data described at the top of this article
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))

site_id    <- "AL_SITE_19"
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

## -----------------------------------------------------------------------------
tracts <- attr(weighted, "cacs_tract_audit") |>
  transmute(
    GEOID,
    int_area_m2,
    tract_area_m2,
    w_cov  = int_area_m2 / tract_area_m2,            # coverage weight
    w_mean = int_area_m2 / sum(int_area_m2)          # area share
  ) |>
  arrange(desc(int_area_m2))

tracts

## -----------------------------------------------------------------------------
two_rows <- weighted |>
  filter(variable %in% c("B17001_002", "B19301_001")) |>
  select(variable, estimate, moe, weight_sum, estimand_family, weight_basis)

two_rows

## -----------------------------------------------------------------------------
pc_income <- acs |>
  sf::st_drop_geometry() |>
  filter(GEOID %in% tracts$GEOID, variable == "B19301_001") |>
  select(GEOID, X = estimate)

demo <- tracts |>
  select(GEOID, w_cov, w_mean) |>
  left_join(pc_income, by = "GEOID")

demo

pc_sum <- sum(demo$w_cov  * demo$X)   # coverage weights, as for a count
pc_avg <- sum(demo$w_mean * demo$X)   # area shares, as the package does

c(coverage_weighted_sum = pc_sum, area_share_average = pc_avg)

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

