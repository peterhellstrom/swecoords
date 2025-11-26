library(tidyverse)
library(sf)
library(parzer)
library(readxl)
library(janitor)
library(devtools)

# Test in new session ----
devtools::load_all()
control_pts_local
control_pts_sweref99_tm
control_pts_rt90_sweref99_tm

# Create data sets ----
path <- "inst/extdata/RT90_SWEREF99_direct_transformation.xlsx"

# Control points local SWEREF99 zones ----
control_pts_local <- read_xlsx(
  path,
  "Control_points_local_zones",
  range = cell_limits(c(2, 1), c(NA, 6))
) |>
  clean_names() |>
  rename(crs = epsg) |>
  mutate(
    latitude = parzer::parse_lat(latitude),
    longitude = parzer::parse_lon(longitude)
  )

control_pts_local
use_data(control_pts_local, overwrite = TRUE)

## Test transformation function ----
control_pts_local |>
  mutate(
    geodetic_to_grid(longitude, latitude, crs)
  ) |>
  filter(
    !near(northing, round(y, 3)) | !near(easting, round(x, 3))
  )

# Control points SWEREF99 TM ----
control_pts_sweref99_tm <- read_xlsx(
  path,
  "Control_points_SWEREF99_TM",
  range = cell_limits(c(2, 1), c(NA, 4))
) |>
  clean_names() |>
  mutate(
    latitude = parzer::parse_lat(latitude),
    longitude = parzer::parse_lon(longitude)
  )

control_pts_sweref99_tm
use_data(control_pts_sweref99_tm, overwrite = TRUE)

## Test transformation function ----
control_pts_sweref99_tm |>
  mutate(
    geodetic_to_grid(longitude, latitude, 3006)
  ) |>
  filter(
    !near(northing, round(y, 3)) | !near(easting, round(x, 3))
  )

# Control points transformation RT90 2.5 gon V <-> SWEREF99 TM ----
control_pts_rt90_sweref99_tm <- read_xlsx(
  path,
  "Control_points_direct_trans (2)",
  range = cell_limits(c(2, 1), c(NA, 5)),
  .name_repair = "unique_quiet"
) |>
  clean_names() |>
  rename(
    northing_3847 = northing_2,
    easting_3847 = easting_3,
    northing_3006 = northing_4,
    easting_3006 = easting_5
  )

control_pts_rt90_sweref99_tm
use_data(control_pts_rt90_sweref99_tm, overwrite = TRUE)

## Test transformation function ----
control_pts_rt90_sweref99_tm |>
  grid_to_grid(easting_3847, northing_3847, 3847, 3006) |>
  filter(
    !near(northing_3006, round(y, 3)) | !near(easting_3006, round(x, 3))
  )

# Maps ----
control_pts_sweref99_tm |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4619,
    remove = FALSE
  ) |>
  mapview(layer.name = "Control points SWEREF99")

control_pts_rt90_sweref99_tm |>
  st_as_sf(
    coords = c("easting_3847", "northing_3847"),
    crs = 3847,
    remove = FALSE
  ) |>
  mapview(layer.name = "Control points 3847 to 3006")
