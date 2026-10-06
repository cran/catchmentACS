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
# Inputs and results of a run with version 0.2
old_run <- readRDS(system.file(
  "testdata", "fixture_032_3site_fresh.rds",
  package = "catchmentACS", mustWork = TRUE
))

# Drive-time areas saved with version 0.2
old_areas <- old_run$iso_sf_v2

# Drive-time areas that pass the current checks
example_areas <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds",
  package = "catchmentACS", mustWork = TRUE
))

## -----------------------------------------------------------------------------
issues <- cacs_validate_iso(old_areas)
issues[, c("check", "col", "actual")]
writeLines(issues$example)

## -----------------------------------------------------------------------------
make_square <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
    c(xmin, ymax), c(xmin, ymin)
  )))
}
core <- make_square(-87.10, 33.00, -87.00, 33.10)

# Two bands: 0 to 5 minutes, and 5 to 10 minutes (a larger square
# with the first one cut out)
bands <- sf::st_sf(
  site_id = "S01",
  isomin = c(0L, 5L),
  isomax = c(5L, 10L),
  geometry = sf::st_sfc(
    core,
    sf::st_difference(make_square(-87.20, 32.90, -86.90, 33.20), core),
    crs = 4326
  )
)

# Share of the shorter area that lies inside the longer one
inside_share <- function(shorter, longer) {
  overlap <- sf::st_intersection(sf::st_geometry(longer),
                                 sf::st_geometry(shorter))
  sum(as.numeric(sf::st_area(overlap))) /
    sum(as.numeric(sf::st_area(shorter)))
}
inside_share(shorter = bands[1, ], longer = bands[2, ])

## -----------------------------------------------------------------------------
cumulative <- cacs_rings_to_cumulative(bands)
sf::st_drop_geometry(cumulative)
inside_share(shorter = cumulative[1, ], longer = cumulative[2, ])

## -----------------------------------------------------------------------------
areas <- cumulative
areas$provider <- "osrm"
areas$provider_requested <- "osrm"
areas$provider_downgrade <- FALSE
areas$profile <- "car"
areas$routing_engine_version <- "osrm-pkg/unknown"
areas$polygon_simplification_tolerance <- NA_real_
areas$osm_snapshot_date <- "unknown"
areas$osm_snapshot_status <- "unknown_best_effort"
areas$generated_at <- Sys.time()
areas$isochrone_empty <- FALSE
areas$failure_reason <- NA_character_
areas$retry_count <- 1L

nrow(cacs_validate_iso(areas))

## -----------------------------------------------------------------------------
bad_crs <- sf::st_transform(example_areas[1:2, ], 4269)
cacs_validate_iso(bad_crs) |>
  dplyr::filter(check == "iso_crs") |>
  dplyr::select(check, actual, expected, example)

## -----------------------------------------------------------------------------
bad_type <- example_areas[1, ]
bad_type$provider_downgrade <- NA_character_
issues <- cacs_validate_iso(bad_type)
issues[, c("col", "actual", "expected")]
writeLines(issues$example)

## -----------------------------------------------------------------------------
old_sites <- sf::st_drop_geometry(old_run$sites_sf)
old_sites <- old_sites[, c("site_id", "lon", "lat")]
old_run_areas <- old_run$iso_sf_v2
old_run_areas$ring_topology <- "cumulative"

rerun <- cacs_run(
  old_sites,
  state = "AL",
  drive_times = 10,
  precomputed_isochrones = old_run_areas,
  acs = old_run$acs_sf,
  weight_args = list(keep_tract_audit = TRUE),
  verbose = FALSE
)

## -----------------------------------------------------------------------------
compare <- dplyr::inner_join(
  tibble::as_tibble(old_run$result_run_v2) |>
    dplyr::select(site_id, variable, saved = estimate),
  tibble::as_tibble(rerun) |>
    dplyr::select(site_id, variable, new = estimate),
  by = c("site_id", "variable")
)
dplyr::filter(
  compare,
  site_id == "AL_BHM_01",
  variable %in% c("B01003_001", "B19013_001", "B19301_001",
                  "poverty_rate", "ssi_rate")
)

# All other counts and rates at the three sites
same <- dplyr::filter(
  compare,
  !variable %in% c("B19013_001", "B19301_001", "ssi_rate")
)
all.equal(same$saved, same$new)

## -----------------------------------------------------------------------------
proxies <- dplyr::filter(compare, variable %in% c("B19013_001", "B19301_001"))
dplyr::mutate(proxies, ratio = new / saved)

## -----------------------------------------------------------------------------
cacs_acs_default_rates$ssi_rate
setdiff(unlist(cacs_acs_default_rates), old_run$acs_sf$variable)

## -----------------------------------------------------------------------------
tracts <- attr(rerun, "cacs_tract_audit")
names(tracts)
dplyr::count(tracts, site_id, drive_time_min, name = "tracts")

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

