## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  message = FALSE,
  fig.width = 7,
  fig.height = 5,
  out.width = "100%"
)

## -----------------------------------------------------------------------------
library(catchmentACS)

## -----------------------------------------------------------------------------
# Compute every result in this article instead of reading saved ones; the
# option is restored at the end of the article.
old_options <- options(catchmentACS.cache_enabled = FALSE)

## -----------------------------------------------------------------------------
walkthrough <- readRDS(system.file("extdata", "visual_walkthrough_fixture.rds",
                                   package = "catchmentACS"))
names(walkthrough)
one_site <- walkthrough$anchor_site
one_site

## -----------------------------------------------------------------------------
cacs_plot_site_isochrone(
  site_id  = one_site,
  iso_sf   = walkthrough$iso_sf,
  sites_df = walkthrough$sites_df
)

## -----------------------------------------------------------------------------
cacs_plot_site_intersection(
  site_id  = one_site,
  iso_sf   = walkthrough$iso_sf,
  tract_sf = walkthrough$tract_sf,
  sites_df = walkthrough$sites_df
)

## -----------------------------------------------------------------------------
cacs_plot_site_weighted(
  site_id  = one_site,
  iso_sf   = walkthrough$iso_sf,
  tract_sf = walkthrough$tract_sf,
  acs_sf   = walkthrough$acs_sf,
  variable = "B17001_002",
  sites_df = walkthrough$sites_df
)

## -----------------------------------------------------------------------------
cacs_plot_site_rates(
  site_id    = one_site,
  iso_sf     = walkthrough$iso_sf,
  run_result = walkthrough$run_result,
  sites_df   = walkthrough$sites_df
)

## -----------------------------------------------------------------------------
maps <- cacs_plot_site_pipeline(
  site_id    = one_site,
  iso_sf     = walkthrough$iso_sf,
  tract_sf   = walkthrough$tract_sf,
  acs_sf     = walkthrough$acs_sf,
  run_result = walkthrough$run_result,
  sites_df   = walkthrough$sites_df,
  variable   = "B17001_002"
)
names(maps)

## -----------------------------------------------------------------------------
# The map of cacs_plot_site_rates(), as in Stage 4
maps$rates

## -----------------------------------------------------------------------------
options(old_options)
rm(old_options)

