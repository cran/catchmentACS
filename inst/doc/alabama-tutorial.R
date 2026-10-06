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
library(tidyr)

## -----------------------------------------------------------------------------
# Compute every result in this article instead of reading saved ones; the
# option is restored at the end of the article.
old_options <- options(catchmentACS.cache_enabled = FALSE)

## -----------------------------------------------------------------------------
cacs_alabama_sites |>
  sf::st_drop_geometry() |>
  select(site_id, site_name, county_fips, region_label)

## -----------------------------------------------------------------------------
sf::st_crs(cacs_alabama_sites)$epsg

# X is the longitude and Y the latitude of each point.
data.frame(
  site_id = cacs_alabama_sites$site_id,
  sf::st_coordinates(cacs_alabama_sites)
)

## -----------------------------------------------------------------------------
site_ids <- c("AL_SITE_07", "AL_SITE_08", "AL_SITE_11")

## -----------------------------------------------------------------------------
# # Needs a Census API key and an internet connection.
# acs <- cacs_acs_prefetch(state = "AL", year = 2023)

## -----------------------------------------------------------------------------
# # Needs the osrm package and an internet connection; the public OSRM server
# # needs no key.
# iso <- cacs_isochrone(
#   sites       = cacs_alabama_sites,
#   drive_times = c(5, 10, 15),
#   provider    = "osrm",
#   osrm_mode   = "demo"
# )

## -----------------------------------------------------------------------------
# The three example files described above
sites <- readRDS(system.file(
  "extdata", "legacy_2025_sites.rds", package = "catchmentACS"
))
acs <- readRDS(system.file(
  "extdata", "sample_alabama_subset.rds", package = "catchmentACS"
))
iso <- readRDS(system.file(
  "extdata", "legacy_2025_isochrones.rds", package = "catchmentACS"
))

# Keep the three sites and the drive times of 5, 10, and 15 minutes.
analysis_sites <- sites |>
  filter(site_id %in% site_ids)
iso <- iso |>
  filter(site_id %in% site_ids, drive_time_min %in% c(5L, 10L, 15L))

list(
  acs_dim         = dim(acs),
  acs_crs         = sf::st_crs(acs)$epsg,
  acs_variables   = length(unique(acs$variable)),
  acs_tracts      = length(unique(acs$GEOID)),
  iso_dim         = dim(iso),
  iso_crs         = sf::st_crs(iso)$epsg
)

## -----------------------------------------------------------------------------
weighted <- cacs_intersect_weight(
  iso_sf        = iso,
  acs_sf        = acs,
  weight_method = "area",
  verbose       = FALSE
)

dim(weighted)

## -----------------------------------------------------------------------------
area_11 <- weighted |>
  filter(site_id == "AL_SITE_11", drive_time_min == 10) |>
  select(variable, estimate, weight_basis, weight_sum, n_tracts)

area_11

## -----------------------------------------------------------------------------
with_moe <- cacs_propagate_moe(weighted, verbose = FALSE)

with_moe |>
  select(site_id, drive_time_min, variable, estimate, moe,
         moe_formula_effective) |>
  arrange(site_id, drive_time_min, variable) |>
  head(8)

## -----------------------------------------------------------------------------
final <- cacs_derive_rates(with_moe, verbose = FALSE)

dim(final)

## -----------------------------------------------------------------------------
final |>
  filter(estimand_family == "derived_rate") |>
  select(site_id, drive_time_min, variable, estimate, moe,
         moe_formula_effective) |>
  arrange(site_id, drive_time_min, variable) |>
  head(10)

## -----------------------------------------------------------------------------
result <- cacs_run(
  sites                  = analysis_sites,
  state                  = "AL",
  year                   = 2023,
  drive_times            = c(5, 10, 15),
  variables              = unname(cacs_acs_default_vars),
  provider               = "osrm",
  precomputed_isochrones = iso,
  acs                    = acs,
  weight_method          = "area",
  output                 = "long",
  verbose                = FALSE
)

class(result)
dim(result)

## -----------------------------------------------------------------------------
manual <- final |>
  arrange(site_id, drive_time_min, variable)
oneshot <- tibble::as_tibble(result) |>
  arrange(site_id, drive_time_min, variable)

cols <- c("site_id", "drive_time_min", "variable", "estimate", "moe")
all.equal(as.data.frame(manual)[cols], as.data.frame(oneshot)[cols])

## -----------------------------------------------------------------------------
result_tbl <- tibble::as_tibble(result)

report_cols <- c(
  "site_id", "drive_time_min", "variable",
  "estimate", "moe", "n_tracts", "failure_origin"
)

result_tbl |>
  select(all_of(report_cols)) |>
  head(12)

## -----------------------------------------------------------------------------
rate_matrix <- result_tbl |>
  filter(variable %in% names(cacs_acs_default_rates)) |>
  select(site_id, drive_time_min, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  arrange(site_id, drive_time_min)

print(rate_matrix, width = Inf)

## -----------------------------------------------------------------------------
print(summary(result)$rates_per_site_moe, width = Inf)

## -----------------------------------------------------------------------------
ranked_15 <- result_tbl |>
  filter(
    drive_time_min == 15,
    variable == "poverty_rate",
    failure_origin == "none"
  ) |>
  arrange(desc(estimate)) |>
  transmute(
    rank         = row_number(),
    site_id,
    poverty_rate = estimate,
    moe_90       = moe
  )

ranked_15

## -----------------------------------------------------------------------------
# Each site against the next one in the ranking, at the 90 percent level
ranking_test <- ranked_15 |>
  mutate(
    se         = moe_90 / 1.645,
    next_site  = lead(site_id),
    difference = poverty_rate - lead(poverty_rate),
    threshold  = 1.645 * sqrt(se^2 + lead(se)^2),
    differ     = difference > threshold
  ) |>
  filter(!is.na(next_site)) |>
  select(site_id, next_site, difference, threshold, differ)

ranking_test

## -----------------------------------------------------------------------------
# suppressWarnings() hides a warning that counts the rows for which the ratio
# formula replaced the proportion formula.
rates_auto <- suppressWarnings(
  cacs_derive_rates(with_moe, formula_dispatch = "auto", verbose = FALSE)
)

ranking_auto <- rates_auto |>
  filter(
    drive_time_min == 15,
    variable == "poverty_rate",
    failure_origin == "none"
  ) |>
  arrange(desc(estimate)) |>
  mutate(
    se         = moe / 1.645,
    next_site  = lead(site_id),
    difference = estimate - lead(estimate),
    threshold  = 1.645 * sqrt(se^2 + lead(se)^2),
    differ     = difference > threshold
  ) |>
  select(site_id, moe, moe_fallback, next_site, difference, threshold, differ)

ranking_auto

## -----------------------------------------------------------------------------
focal_site <- "AL_SITE_08"

result_tbl |>
  filter(
    site_id == focal_site,
    variable %in% names(cacs_acs_default_rates)
  ) |>
  arrange(variable, drive_time_min) |>
  transmute(
    variable,
    drive_time_min,
    estimate,
    moe_90  = moe,
    ci_low  = estimate - moe,
    ci_high = estimate + moe
  )

## -----------------------------------------------------------------------------
export_tbl <- result_tbl |>
  filter(variable %in% names(cacs_acs_default_rates)) |>
  transmute(
    site_id,
    drive_time_min,
    variable,
    estimate,
    moe_90                = moe,
    estimate_pct          = 100 * estimate,
    moe_90_pct            = 100 * moe,
    failure_origin,
    moe_formula_effective,
    moe_fallback
  ) |>
  arrange(site_id, drive_time_min, variable)

head(export_tbl, 15)

## -----------------------------------------------------------------------------
# The site and its 15-minute area (both in EPSG:4326)
focal_pt <- analysis_sites[analysis_sites$site_id == focal_site, ]
area_15  <- iso[iso$site_id == focal_site & iso$drive_time_min == 15, ]

# The poverty rate of each tract, from its ACS counts, for the tracts that
# overlap the 15-minute area.
pov <- acs |>
  sf::st_drop_geometry() |>
  filter(variable %in% c("B17001_001", "B17001_002")) |>
  select(GEOID, variable, estimate) |>
  tidyr::pivot_wider(
    id_cols = GEOID, names_from = variable, values_from = estimate
  ) |>
  transmute(GEOID, poverty_rate = B17001_002 / B17001_001)

tract_geom <- acs |>
  filter(variable == "B17001_001") |>
  select(GEOID) |>
  left_join(pov, by = "GEOID") |>
  sf::st_transform(4326)

squares <- tract_geom[
  sf::st_intersects(tract_geom, area_15, sparse = FALSE)[, 1],
]

pal <- leaflet::colorNumeric("YlOrRd", domain = squares$poverty_rate)

leaflet::leaflet(squares) |>
  leaflet::addProviderTiles("OpenStreetMap") |>
  leaflet::addPolygons(
    fillColor = ~pal(poverty_rate), fillOpacity = 0.6,
    weight = 0.5, color = "#666666",
    label = ~sprintf("Tract %s: %.1f%%", GEOID, 100 * poverty_rate)
  ) |>
  leaflet::addPolygons(
    data = area_15, fill = FALSE,
    weight = 2, opacity = 0.8, color = "#c05621"
  ) |>
  leaflet::addCircleMarkers(
    data = focal_pt, radius = 5, color = "#2b6cb0",
    fillOpacity = 0.9, stroke = FALSE, popup = ~site_id
  ) |>
  leaflet::addLegend(
    pal = pal, values = ~poverty_rate,
    title = "Tract poverty rate",
    labFormat = leaflet::labelFormat(transform = function(x) 100 * x, suffix = "%")
  )

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

