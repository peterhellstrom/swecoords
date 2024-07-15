# https://r-pkgs.org
# devtools::install_github("r-lib/devtools")
# devtools::install_github("r-lib/usethis")

# can be added to .Rprofile startup file
library(devtools)

# Create project ----
# p <- "W:/projects/R/swecoords"
# usethis::create_package(p, check_name = FALSE)

# License ----
# usethis::use_mit_license()

# Creat GitHub repository ----
# use_git_config(
#   user.name = "peterhellstrom",
#   user.email = "peter.hellstrom@nrm.se"
# )
# usethis::use_git()
# usethis::use_github()
#
# usethis::create_github_token()

# Load all ----
load_all()

# Documentation / NAMESPACE ----
document()

# Check ----
chk_pkg <- check()
dplyr::glimpse(chk_pkg)
names(chk_pkg)

chk_pkg$checkdir
utils::browseURL(chk_pkg$checkdir)

# Test ----
test()

# ReadMe ----
use_readme_rmd()
build_readme()

# Imports ----
# How to deal with this?
# Imports includes {n} non-default packages.
# Importing from so many packages makes the package vulnerable to any of
# them becoming unavailable.  Move as many as possible to Suggests and
# use conditionally.

# https://stackoverflow.com/questions/63345284/r-package-cran-note-for-package-dependencies-and-warnings-in-tests
# Bad practice to import entire package?
# Use some functions "conditionally"?

# 1) Check if these are necessary or can be replaced:
# arcgisbinding, gdalUtils, gtools, oce
# 2) And add dependencies to non-CRAN packages,
# like my own; eagles
# 3) When should ImportFrom be used?
# 4) How to use requireNamespace
#    Use `requireNamespace("ggplot2", quietly = TRUE)` to test if package is installed
#    • Then directly refer to functions with `ggplot2::fun()`

usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("glue", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)
usethis::use_package("readr", min_version = TRUE)
usethis::use_package("rlang", min_version = TRUE)
usethis::use_package("rvest", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("tibble", min_version = TRUE)
usethis::use_package("tidyr", min_version = TRUE)
usethis::use_package("tidyselect", min_version = TRUE)
usethis::use_package("unglue", min_version = TRUE)
usethis::use_package("XML", min_version = TRUE)
usethis::use_package("xml2", min_version = TRUE)

usethis::use_package("geosphere", min_version = TRUE)
usethis::use_package("igraph", min_version = TRUE)
usethis::use_package("leafem", min_version = TRUE)
usethis::use_package("leaflet", min_version = TRUE)
usethis::use_package("leaflet.extras", min_version = TRUE)
usethis::use_package("rmapshaper", min_version = TRUE)
usethis::use_package("sf", min_version = TRUE)
usethis::use_package("units", min_version = TRUE)

usethis::use_package("DBI", min_version = TRUE)
usethis::use_package("RSQLite", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)

# Suggests ----
usethis::use_package("ggplot2", "Suggests")
usethis::use_package("writexl", "Suggests")
usethis::use_package("tmap", "Suggests")
usethis::use_package("terra", "Suggests")

requireNamespace("ggplot2", quietly = TRUE)

usethis::use_tidy_description()

# Install ----
install()

## Install from GitHub ----
# install_github("peterhellstrom/swecoords")

# Ignore ----
usethis::use_build_ignore(
  c("backup", "data-raw", "development", "examples")
)

# Document data ----
# https://r-pkgs.org/data.html

## Load package ----
library(swecoords)
utils::sessionInfo()
sessioninfo::session_info()

## Data sets ----
usethis::use_data_raw()

storrutor
ekorutor
fastighetsblad
wms_layers_data
tms_layers_data

storrutor |>
  sf::st_as_sf(
    coords = c("easting", "northing"),
    crs = 3021
  ) |>
  mapview::mapview()

grid_rt_90 <- function(.data, grid_size, crs = 3021) {
  .data |>
    dplyr::mutate(
      geometry = purrr::map2(
        easting, northing,
        \(x, y) grid_cell(x, y, grid_size, grid_size)
      )
    ) |>
    sf::st_as_sf(crs = crs)
}

grid_rt_90(storrutor, 50000) |>
  mapview::mapview()

grid_rt_90(ekorutor, 5000) |>
  mapview::mapview()

lm_basemaps()
swe_tiles(tile_providers = tms_layers_data)
