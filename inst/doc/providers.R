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
# tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)

## -----------------------------------------------------------------------------
# # Needs the osrm package and an internet connection.
# iso <- cacs_isochrone(
#   sites = cacs_alabama_sites[1:3, ],
#   drive_times = c(5, 10, 15),
#   provider = "osrm",
#   osrm_mode = "demo",
#   res = 30L,
#   verbose = TRUE
# )

## -----------------------------------------------------------------------------
# # Needs the osrm package and an OSRM server at this address.
# iso <- cacs_isochrone(
#   sites = cacs_alabama_sites[1:3, ],
#   drive_times = c(5, 10, 15),
#   provider = "osrm",
#   osrm_mode = "docker",
#   `osrm.server` = "http://localhost:5000/",
#   verbose = TRUE
# )

## -----------------------------------------------------------------------------
# # Needs an internet connection for the first call and a server that you
# # run for the second.
# cacs_validate_osrm_endpoint()
# cacs_validate_osrm_endpoint(server = "http://localhost:5000", timeout = 2)

## -----------------------------------------------------------------------------
# # Needs the openrouteservice package, an API key, and an internet connection.
# nzchar(Sys.getenv("ORS_API_KEY"))  # TRUE when the key is set
# 
# iso <- cacs_isochrone(
#   sites = cacs_alabama_sites[1:3, ],
#   drive_times = c(5, 10, 15),
#   provider = "ors",
#   ors_api_key = Sys.getenv("ORS_API_KEY"),
#   verbose = TRUE
# )

## -----------------------------------------------------------------------------
example_iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))

example_iso |>
  sf::st_drop_geometry() |>
  count(provider, provider_requested, profile,
        osm_snapshot_date, ring_topology)

## -----------------------------------------------------------------------------
example_sites <- readRDS(system.file(
  "extdata", "legacy_2025_sites.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))

one_site <- "AL_SITE_07"
iso_one <- example_iso |>
  filter(site_id == one_site, drive_time_min == 5L)
site_one <- example_sites |>
  filter(site_id == one_site)

out <- cacs_run(
  sites = site_one,
  state = "AL",
  year = 2023,
  drive_times = 5L,
  variables = unname(cacs_acs_default_vars),
  provider = "osrm",
  precomputed_isochrones = iso_one,
  acs = acs,
  weight_method = "area",
  output = "long",
  verbose = FALSE
)

attr(out, "cacs_run_provenance") |>
  as.data.frame() |>
  select(execution_path, provider, bypass_iso, bypass_acs,
         weight_method, output_format)

## -----------------------------------------------------------------------------
# # Lists what the cache folder holds, then deletes the saved drive-time
# # areas and ACS estimates.
# cacs_cache_status()
# cacs_clear_cache("isochrone", confirm = FALSE)
# cacs_clear_cache("acs", confirm = FALSE)

## -----------------------------------------------------------------------------
# # Needs the osrm package, a Census API key, and an internet connection.
# result <- cacs_run(
#   sites = cacs_alabama_sites,  # or your own sites
#   state = "AL",
#   year = 2023,
#   drive_times = c(5, 10, 15),
#   variables = unname(cacs_acs_default_vars),
#   provider = "osrm",
#   iso_args = list(osrm_mode = "demo"),  # "docker" for a server that you run
#   # With "docker", the server's address is set before the call with
#   # options(catchmentACS.osrm_docker_server = "http://localhost:5000/")
#   weight_method = "area",
#   output = "long",
#   verbose = TRUE
# )
# 
# summary(result)$rates_per_site_moe
# attr(result, "cacs_run_provenance")

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

