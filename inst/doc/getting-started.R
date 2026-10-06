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
# Load the three bundled example inputs.
sites <- readRDS(system.file(
  "extdata", "legacy_2025_sites.rds", package = "catchmentACS"
))
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))

one_site <- "AL_SITE_07"

result <- cacs_run(
  sites                  = sites[sites$site_id == one_site, , drop = FALSE],
  state                  = "AL",
  precomputed_isochrones = iso[iso$site_id == one_site, , drop = FALSE],
  acs                    = acs,
  verbose                = FALSE
)

dim(result)

## -----------------------------------------------------------------------------
# result_live <- cacs_run(
#   sites       = cacs_alabama_sites,   # or your own sites
#   state       = "AL",
#   year        = 2023,
#   drive_times = c(5, 10, 15),
#   provider    = "osrm",
#   output      = "long"
# )

## -----------------------------------------------------------------------------
rows_15 <- tibble::as_tibble(result) |>
  filter(drive_time_min == 15) |>
  select(variable, estimate, moe, failure_origin)
rows_15

## -----------------------------------------------------------------------------
poverty_15 <- filter(rows_15, variable == "poverty_rate")
c(lower = poverty_15$estimate - poverty_15$moe,
  upper = poverty_15$estimate + poverty_15$moe)

## -----------------------------------------------------------------------------
table(failure_origin = result$failure_origin,
      missing_estimate = is.na(result$estimate))

## -----------------------------------------------------------------------------
print(summary(result)$rates_per_site_moe, width = Inf)

## -----------------------------------------------------------------------------
tibble::as_tibble(result) |>
  filter(variable == "B01003_001") |>
  select(drive_time_min, estimate, moe, n_tracts, weight_sum)

## -----------------------------------------------------------------------------
cacs_plot_site_isochrone(
  site_id    = one_site,
  iso_sf     = iso,
  sites_df   = sites,
  padding_km = 20
)

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

