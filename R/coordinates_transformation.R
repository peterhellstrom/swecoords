# Direct transformation RT 90 <-> SWEREF 99
# using Gauss-Krügers projection

# https://www.lantmateriet.se/sv/Kartor-och-geografisk-information/gps-geodesi-och-swepos/Om-geodesi/Geodesitjanster/enkel-koordinattransformation/
# Inspired by javascripts from:
# http://latlong.mellifica.se/
# http://mellifica.se/geodesi/gausskruger.js
# https://rl.se/rt90

# https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/#a-simpler-interpolation-pattern-with

# Last updated 2023-02-23

# To do: tidy up input and output parameters, handle order x & y
# in a more consistent way.

#' Title
#'
#' @param angle
#'
#' @return
#' @export
#'
#' @examples
deg_to_rad <- function(angle) {
  angle * (base::pi / 180)
}

#' Title
#'
#' @param radians
#'
#' @return
#' @export
#'
#' @examples
rad_to_deg <- function(radians) {
  radians * (180 / base::pi)
}

# y = ?, x = ?

#' Title
#'
#' @param y
#' @param x
#' @param crs
#' @param colnames
#' @param p
#'
#' @return
#' @export
#'
#' @examples
grid_to_geodetic <- function(
    y, x, crs,
    colnames = c("latitude", "longitude"),
    p = NULL) {

  if (is.null(p)) {
    p <- swedish_parameters(crs)
  }

  e2 <- p$f * (2 - p$f)
  n <- p$f / (2 - p$f)
  a_roof <- p$a / (1 + n) * (1 + n^2/4 + n^4/64)

  delta1 <- n / 2 - 2 * n^2/3 + 37 * n^3/96 - n^4/360
  delta2 <- n^2/48 + n^3/15 - 437 * n^4/1440
  delta3 <- 17 * n^3/480 - 37 * n^4/840
  delta4 <- 4397 * n^4/161280

  Astar <- e2 + e2^2 + e2^3 + e2^4
  Bstar <- -(7 * e2^2 + 17 * e2^3 + 30 * e2^4) / 6
  Cstar <- (224 * e2^3 + 889 * e2^4) / 120
  Dstar <- -(4279 * e2^4) / 1260

  lambda_zero <- deg_to_rad(p$lon_of_origin)
  xi <- (x - p$false_northing) / (p$scale * a_roof)
  eta <- (y - p$false_easting) / (p$scale * a_roof)

  xi_prim <- xi -
    delta1 * sin(2 * xi) * cosh(2 * eta) -
    delta2 * sin(4 * xi) * cosh(4 * eta) -
    delta3 * sin(6 * xi) * cosh(6 * eta) -
    delta4 * sin(8 * xi) * cosh(8 * eta)

  eta_prim <- eta -
    delta1 * cos(2 * xi) * sinh(2 * eta) -
    delta2 * cos(4 * xi) * sinh(4 * eta) -
    delta3 * cos(6 * xi) * sinh(6 * eta) -
    delta4 * cos(8 * xi) * sinh(8 * eta)

  phi_star <- asin(sin(xi_prim) / cosh(eta_prim))
  delta_lambda <- atan(sinh(eta_prim) / cos(xi_prim))

  lon_radian <- lambda_zero + delta_lambda
  lat_radian <- phi_star + sin(phi_star) * cos(phi_star) *
    (Astar +
       Bstar * sin(phi_star)^2 +
       Cstar * sin(phi_star)^4 +
       Dstar * sin(phi_star)^6)

  latitude <- rad_to_deg(lat_radian)
  longitude <- rad_to_deg(lon_radian)

  out <- data.frame(latitude, longitude)
  stats::setNames(out, colnames)

}

# y = ?, x = ?

#' Title
#'
#' @param longitude
#' @param latitude
#' @param crs
#' @param colnames
#' @param p
#'
#' @return
#' @export
#'
#' @examples
geodetic_to_grid <- function(
    longitude, latitude, crs,
    colnames = c("y", "x"),
    p = NULL) {

  if (is.null(p)) {
    p <- swedish_parameters(crs)
  }

  e2 <- p$f * (2 - p$f)
  n <- p$f / (2 - p$f)
  a_roof <- p$a / (1 + n) * (1 + n^2/4 + n^4/64)

  A <- e2
  B <- (5 * e2^2 - e2^3) / 6
  C <- (104 * e2^3 - 45 * e2^4) / 120
  D <- (1237 * e2^4) / 1260

  beta1 <- n/2 - 2 * n^2/3 + 5 * n^3/16 + 41 * n^4/180
  beta2 <- 13 * n^2/48 - 3 * n^3/5 + 557 * n^4/1440
  beta3 <- 61 * n^3/240 - 103 * n^4/140
  beta4 <- 49561 * n^4/161280

  phi <- deg_to_rad(latitude)
  lambda <- deg_to_rad(longitude)
  lambda_zero <- deg_to_rad(p$lon_of_origin)

  phi_star <- phi - sin(phi) * cos(phi) *
    (A +
       B * sin(phi)^2 +
       C * sin(phi)^4 +
       D * sin(phi)^6)

  delta_lambda <- lambda - lambda_zero
  xi_prim <- atan(tan(phi_star) / cos(delta_lambda))
  eta_prim <- atanh(cos(phi_star) * sin(delta_lambda))

  x <- p$scale * a_roof * (
    xi_prim +
      beta1 * sin(2 * xi_prim) * cosh(2 * eta_prim) +
      beta2 * sin(4 * xi_prim) * cosh(4 * eta_prim) +
      beta3 * sin(6 * xi_prim) * cosh(6 * eta_prim) +
      beta4 * sin(8 * xi_prim) * cosh(8 * eta_prim)) +
    p$false_northing

  y <- p$scale * a_roof * (
    eta_prim +
      beta1 * cos(2 * xi_prim) * sinh(2 * eta_prim) +
      beta2 * cos(4 * xi_prim) * sinh(4 * eta_prim) +
      beta3 * cos(6 * xi_prim) * sinh(6 * eta_prim) +
      beta4 * cos(8 * xi_prim) * sinh(8 * eta_prim)) +
    p$false_easting

  out <- data.frame(x, y)
  stats::setNames(out, colnames)
}

#' Title
#'
#' @param north
#' @param east
#'
#' @return
#' @export
#'
#' @examples
detect_crs <- function(north, east) {
  dplyr::case_when(
    between(north, 6100000, 7700000) & between(east, 1200000, 1900000) ~ 3847,
    between(north, 6000000, 7700000) & between(east, 200000, 1000000) ~ 3006,
    between(north, 54, 70) & between(east, 10, 26) ~ 4326
  )
}

# Wrapper function(s) for direct transformation

#' Title
#'
#' @param .data
#' @param y
#' @param x
#' @param crs_from
#' @param crs_to
#' @param round
#' @param round_grid
#' @param round_degrees
#' @param colnames_new
#'
#' @return
#' @export
#'
#' @examples
grid_to_grid <- function(
    .data,
    y,
    x,
    crs_from,
    crs_to,
    round = FALSE,
    round_grid = 0,
    round_degrees = 6,
    colnames_new = c("y", "x")
) {

  out <- .data |>
    dplyr::mutate(
      grid_to_geodetic( {{ y }}, {{ x }}, {{ crs_from }} )
    )

  out <- out |>
    dplyr::mutate(
      dplyr::case_when(
        {{ crs_from }} == crs_to ~ data.frame( {{y}}, {{x}} ) |>
          rlang::set_names(colnames_new),
        TRUE ~ geodetic_to_grid(longitude, latitude, {{ crs_to }}, colnames_new)
      )
    )

  if (round) {
    out <- out |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(colnames_new), \(x) round(x, round_grid)
        ),
        dplyr::across(
          c(latitude, longitude), \(x) round(x, round_degrees)
        )
      )
  }
  out
}

# 1) convert everything to geodetic
# 2) convert from geodetic to grid based on vector with crs's.
# append crs to standardized column name
# all datavalues are transformed
# 3) retain original data, i.e. replace transformed data
# with original input data if input crs == output crs
# 4) extras: possible to customize column names

#' Title
#'
#' @param ost
#' @param nord
#' @param crs_from
#' @param crs_to
#' @param colnames_geodetic
#' @param colnames_grid_prefix
#' @param colnames_grid
#' @param round
#' @param round_grid
#' @param round_degrees
#' @param keep_original
#'
#' @return
#' @export
#'
#' @examples
swe_transform <- function(
    ost,
    nord,
    crs_from,
    crs_to = c(3006, 3847),
    colnames_geodetic = c("latitude", "longitude"),
    colnames_grid_prefix = c("north", "east"),
    colnames_grid = purrr::map(
      crs_to,
      \(x) str_c(colnames_grid_prefix, x, sep = "_")
    ),
    round = FALSE,
    round_grid = 0,
    round_degrees = 6,
    keep_original = TRUE
) {

  d_geodetic <- grid_to_geodetic(
    {{ ost }} , {{ nord }}, crs_from, colnames_geodetic
  )

  if (keep_original) {
    d_geodetic[[colnames_geodetic[1]]] <-
      ifelse(
        is.na(d_geodetic[[colnames_geodetic[1]]]),
        {{ nord }},
        d_geodetic[[colnames_geodetic[1]]]
      )

    d_geodetic[[colnames_geodetic[2]]] <-
      ifelse(
        is.na(d_geodetic[[colnames_geodetic[2]]]),
        {{ ost }},
        d_geodetic[[colnames_geodetic[2]]]
      )
  }

  d_grid <- purrr::map2(
    crs_to,
    colnames_grid,
    \(x, y) geodetic_to_grid(d_geodetic[,2], d_geodetic[,1], x, y)
  )

  d_out <- dplyr::bind_cols(d_geodetic, d_grid)

  if (keep_original) {
    if (length(crs_from) == 1) {
      crs_from <- rep(crs_from, nrow(d_out))
    }

    for (i in seq_along(crs_to)) {
      d_out[[colnames_grid[[i]][1]]] <-
        ifelse(
          crs_from == crs_to[i],
          nord,
          d_out[[colnames_grid[[i]][1]]]
        )
      d_out[[colnames_grid[[i]][2]]] <-
        ifelse(
          crs_from == crs_to[i],
          ost,
          d_out[[colnames_grid[[i]][2]]]
        )
    }
  }

  if (round) {
    d_out <- d_out |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(unlist(colnames_grid)), \(x) round(x, round_grid)
        ),
        dplyr::across(
          tidyselect::all_of(colnames_geodetic), \(x) round(x, round_degrees))
      )
  }

  d_out

}
