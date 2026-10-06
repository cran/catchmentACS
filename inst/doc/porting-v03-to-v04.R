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
iso_07 <- iso[iso$site_id == "AL_SITE_07", ]
site_07 <- data.frame(site_id = "AL_SITE_07", lon = -85.365, lat = 31.655)

## -----------------------------------------------------------------------------
out <- cacs_capture_conditions(
  cacs_run(site_07, state = "AL",
           precomputed_isochrones = iso_07, acs = acs),
  return_value = "both"
)
result <- out$result
table(out$conditions$class)

## -----------------------------------------------------------------------------
# # Needs the osrm package and an internet connection.
# iso_70 <- cacs_isochrone(site_07, drive_times = c(5, 10, 15), res = 70L)

## -----------------------------------------------------------------------------
by_area <- cacs_run(
  site_07, state = "AL", precomputed_isochrones = iso_07, acs = acs,
  output = "list_column", verbose = FALSE
)
class(by_area$isochrone[[1]])

## -----------------------------------------------------------------------------
head(tibble::as_tibble(result)$variable)
head(tibble::as_tibble(result, rate_first = TRUE)$variable)

## -----------------------------------------------------------------------------
cacs_summary_as_markdown(summary(result)$rates_per_site_moe)

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

