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
tracts <- data.frame(
  tract      = c(1, 2),
  area_km2   = c(1, 100),       # area of the tract
  inside_km2 = c(1, 10),        # area of its part inside the drive-time area
  residents  = c(1000, 1000),
  income_pc  = c(10000, 50000)  # per capita income, dollars
)
tracts$w_cov  <- tracts$inside_km2 / tracts$area_km2         # coverage weight
tracts$w_mean <- tracts$inside_km2 / sum(tracts$inside_km2)  # area share
tracts

# The package's formula: the average of the tract values weighted by area shares
area_share_average <- sum(tracts$w_mean * tracts$income_pc)

# The people inside and their per capita income (both assumed even within each tract)
residents_inside <- sum(tracts$w_cov * tracts$residents)
income_inside    <- sum(tracts$w_cov * tracts$residents * tracts$income_pc) /
  residents_inside

c(area_share_average = area_share_average,
  residents_inside   = residents_inside,
  income_inside      = income_inside)

## -----------------------------------------------------------------------------
# Data bundled with the package
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

## -----------------------------------------------------------------------------
areas <- attr(weighted, "cacs_tract_audit") |>
  select(GEOID, int_area_m2, tract_area_m2) |>
  arrange(desc(int_area_m2))

areas

## -----------------------------------------------------------------------------
hand <- areas |>
  mutate(
    w_cov  = int_area_m2 / tract_area_m2,        # |I_s n T_j| / |T_j|
    w_mean = int_area_m2 / sum(int_area_m2)      # |I_s n T_j| / sum_k |I_s n T_k|
  )

hand |> select(GEOID, w_cov, w_mean)

c(
  sum_w_cov  = sum(hand$w_cov),    # not 1 in general: each has its own denominator
  sum_w_mean = sum(hand$w_mean)    # 1 by construction
)

## -----------------------------------------------------------------------------
counts <- acs |>
  sf::st_drop_geometry() |>
  filter(GEOID %in% hand$GEOID, variable == "B17001_002") |>
  select(GEOID, Y = estimate)

hand_total <- hand |>
  select(GEOID, w_cov) |>
  left_join(counts, by = "GEOID")

hand_total

hand_Y_hat       <- sum(hand_total$w_cov * hand_total$Y)   # sum_j w_cov * Y_j
hand_weight_sum  <- sum(hand_total$w_cov)                  # sum_j w_cov

c(hand_Y_hat = hand_Y_hat, hand_weight_sum = hand_weight_sum)

## -----------------------------------------------------------------------------
pkg_row <- weighted |>
  filter(variable == "B17001_002") |>
  select(variable, estimate, weight_sum, n_tracts,
         estimand_family, weight_basis)

pkg_row

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

