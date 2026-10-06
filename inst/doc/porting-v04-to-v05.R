## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  message = FALSE,
  fig.width = 7,
  fig.height = 5
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
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))
site_07 <- data.frame(site_id = "AL_SITE_07", lon = -85.365, lat = 31.655)

result <- cacs_run(
  site_07, state = "AL",
  precomputed_isochrones = iso[iso$site_id == "AL_SITE_07", ],
  acs = acs, verbose = FALSE
)
tibble::as_tibble(result) |>
  filter(weight_basis == "area_mean") |>
  select(drive_time_min, variable, estimate, moe)

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

