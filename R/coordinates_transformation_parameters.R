# Comments on ellipsoid() and swedish_parameters().
# Previous versions were non-vectorized, as
# they internally called switch. Changed from switch
# to dplyr::case_match, which is really fast in R (in comparison to switch),
# but when included in Shiny app,
# it is the opposite... Explanation: performance of switch and case_match
# do scale differently.
# case_match takes almost the same execution time irrespective of sample size
# (and is really useful and fast for data sets with many rows),
# but switch is in fact faster than case_match for small sample sizes!
# This became evident when using geodetic_to_grid in a Shiny app,
# under such circumstances use the new p argument in grid_to_geodetic
# and geodetic_to_grid which allows the user to pre-define a data frame
# with transformation parameter values in order to speed up the
# transformation calculations.

#' Title
#'
#' @param .x
#'
#' @returns
#' @export
#'
#' @examples
ellipsoid <- function(.x) {
  dplyr::case_match(
    .x,
    "GRS 80" ~ data.frame(
      a = 6378137,
      f = 1 / 298.257222101
    ),
    "Bessel 1841" ~ data.frame(
      a = 6377397.155,
      f = 1 / 299.1528128)
  )
}


#' Title
#'
#' @param crs
#'
#' @returns
#' @export
#'
#' @examples
swedish_parameters <- function(crs) {

  dplyr::case_match(
    crs,
    # RT90 parameters, "GRS 80" ellipsoid.
    # Use these when direct transformation <-> SWEREF99
    3845 ~
      data.frame(# rt90_7.5_gon_v
        ellipsoid("GRS 80"),
        lon_of_origin = 11 + 18.375/60,
        lat_of_origin = 0,
        scale = 1.000006000000,
        false_northing = -667.282,
        false_easting = 1500025.141
      ),
    3846 ~
      data.frame(# rt90_5.0_gon_v
        ellipsoid("GRS 80"),
        lon_of_origin = 13 + 33.376/60,
        lat_of_origin = 0,
        scale = 1.000005800000,
        false_northing = -667.130,
        false_easting = 1500044.695
      ),
    3847 ~
      data.frame(# rt90_2.5_gon_v
        ellipsoid("GRS 80"),
        lon_of_origin = 15 + 48/60 + 22.624306/3600,
        lat_of_origin = 0,
        scale = 1.00000561024,
        false_northing = -667.711,
        false_easting = 1500064.274
      ),
    3848 ~
      data.frame(# rt90_0.0_gon_v
        ellipsoid("GRS 80"),
        lon_of_origin = 18 + 3.378/60,
        lat_of_origin = 0,
        scale = 1.000005400000,
        false_northing = -668.844,
        false_easting = 1500083.521
      ),
    3849 ~
      data.frame(# rt90_2.5_gon_o
        ellipsoid("GRS 80"),
        lon_of_origin = 20 + 18.379/60,
        lat_of_origin = 0,
        scale = 1.000005200000,
        false_northing = -670.706,
        false_easting = 1500102.765
      ),
    3850 ~
      data.frame(# rt90_5.0_gon_o
        ellipsoid("GRS 80"),
        lon_of_origin = 22 + 33.380/60,
        lat_of_origin = 0,
        scale = 1.000004900000,
        false_northing = -672.557,
        false_easting = 1500121.846
      ),
    # RT90 parameters, "Bessel 1841" ellipsoid
    3019 ~
      data.frame(# bessel_rt90_7.5_gon_v
        ellipsoid("Bessel 1841"),
        lon_of_origin = 11 + 18/60 + 29.8/3600,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 1500000
      ),
    3020 ~
      data.frame(# bessel_rt90_5.0_gon_v
        ellipsoid("Bessel 1841"),
        lon_of_origin = 13 + 33/60 + 29.8/3600,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 1500000
      ),
    3021 ~
      data.frame(# bessel_rt90_2.5_gon_v
        ellipsoid("Bessel 1841"),
        lon_of_origin = 15 + 48/60 + 29.8/3600,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 1500000
      ),
    3022 ~
      data.frame(# bessel_rt90_0.0_gon_v
        ellipsoid("Bessel 1841"),
        lon_of_origin = 18 + 3/60 + 29.8/3600,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 1500000
      ),
    3023 ~
      data.frame(# bessel_rt90_2.5_gon_o
        ellipsoid("Bessel 1841"),
        lon_of_origin = 20 + 18/60 + 29.8/3600,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 1500000
      ),
    3024 ~
      data.frame(# bessel_rt90_5.0_gon_o
        ellipsoid("Bessel 1841"),
        lon_of_origin = 22 + 33/60 + 29.8/3600,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 1500000
      ),
    # SWEREF99TM and SWEREF99 ddmm parameters, "GRS 80" ellipsoid
    3006 ~
      data.frame(# sweref99_tm
        ellipsoid("GRS 80"),
        lon_of_origin = 15.00,
        lat_of_origin = 0,
        scale = 0.9996,
        false_northing = 0,
        false_easting = 500000
      ),
    3007 ~
      data.frame(# sweref99_1200
        ellipsoid("GRS 80"),
        lon_of_origin = 12.00,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3008 ~
      data.frame(# sweref99_1330
        ellipsoid("GRS 80"),
        lon_of_origin = 13.50,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3012 ~
      data.frame(# sweref99_1415
        ellipsoid("GRS 80"),
        lon_of_origin = 14.25,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3009 ~
      data.frame(# sweref99_1500
        ellipsoid("GRS 80"),
        lon_of_origin = 15.00,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3013 ~
      data.frame(# sweref99_1545
        ellipsoid("GRS 80"),
        lon_of_origin = 15.75,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3010 ~
      data.frame(# sweref99_1630
        ellipsoid("GRS 80"),
        lon_of_origin = 16.50,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3014 ~
      data.frame(# sweref99_1715
        ellipsoid("GRS 80"),
        lon_of_origin = 17.25,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3011 ~
      data.frame(# sweref99_1800
        ellipsoid("GRS 80"),
        lon_of_origin = 18.00,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3015 ~
      data.frame(# sweref99_1845
        ellipsoid("GRS 80"),
        lon_of_origin = 18.75,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3016 ~
      data.frame(# sweref99_2015
        ellipsoid("GRS 80"),
        lon_of_origin = 20.25,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3017 ~
      data.frame(# sweref99_2145
        ellipsoid("GRS 80"),
        lon_of_origin = 21.75,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      ),
    3018 ~
      data.frame(# sweref99_2315
        ellipsoid("GRS 80"),
        lon_of_origin = 23.25,
        lat_of_origin = 0,
        scale = 1,
        false_northing = 0,
        false_easting = 150000
      )
  )
}

#' Title
#'
#' @param crs
#'
#' @returns
#' @export
#'
#' @examples
extract_prj4 <- function(crs) {

  p <- sf::st_crs(crs)

  data.frame(
    code = p$input,
    note = p$Name,
    prj4 = p$proj4string
  )
}

#' Title
#'
#' @param proj_path
#'
#' @returns
#' @export
#'
#' @examples
proj_crs <- function(
    proj_path = base::system.file("proj/proj.db", package = "sf")
) {

  on.exit(DBI::dbDisconnect(con))

  con <- DBI::dbConnect(RSQLite::SQLite(), proj_path)

  crs_table <- purrr::map(
    c("geodetic_crs", "projected_crs"),
    \(x) DBI::dbReadTable(con, x)
  ) |>
    purrr::list_rbind() |>
    tibble::as_tibble()

  crs_table |>
    tibble::as_tibble()
}

#' Title
#'
#' @param .crs
#'
#' @returns
#' @export
#'
#' @examples
crs_prj4 <- function(.crs) {
  purrr::map(
    .crs,
    extract_prj4
  ) |>
    purrr::list_rbind() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      code = readr::parse_number(code) |>
        as.integer()
    ) |>
    dplyr::rename(name = note)
}
