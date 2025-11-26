# Source: https://github.com/r-spatial/sf/issues/231

#' Title
#'
#' @param x
#' @param geometry
#' @param names
#' @param drop_geometry
#'
#' @returns
#' @export
#'
#' @examples
sfc_as_cols <- function(x, geometry, names = c("x","y"), drop_geometry = FALSE) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(rlang::enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- stats::setNames(ret, names)
  x <- dplyr::bind_cols(x,ret)
  if (drop_geometry) {
    x <- x |>
      sf::st_drop_geometry() |>
      dplyr::as_tibble()
  }
  x
}

#' Title
#'
#' @param .x
#' @param coords
#' @param crs
#'
#' @returns
#' @export
#'
#' @examples
st_add_geom_column <- function(.x, coords, crs = 3006) {
  .x |>
    sf::st_as_sf(coords = {{ coords }}, crs = crs) |>
    sf::st_geometry()
}


#' Title
#'
#' @param .x
#' @param .g
#' @param .g_names
#' @param centroid
#' @param crs
#'
#' @returns
#' @export
#'
#' @examples
st_add_geom_column_round <- function(
    .x, .g, .g_names,
    centroid = TRUE,
    crs = 3006
) {

  for (i in seq_along(.g)) {

    .x_coords <- sf::st_coordinates(.x) |>
      tibble::as_tibble()

    .x_coords_round <- .x_coords |>
      dplyr::mutate(
        dplyr::across(X:Y, ~ round_coords(., .g[[i]], centroid))
      )

    .x_col <- sf::st_as_sf(
      .x_coords_round,
      coords = c("X", "Y"),
      crs = crs
    ) |>
      sf::st_geometry()

    .x[[.g_names[i]]] <- .x_col
  }
  .x
}

#' Title
#'
#' @param .x
#'
#' @returns
#' @export
#'
#' @examples
st_extract_pt_coords <- function (.x) {
  # Extract coordinates if input is an sf-object
  if (class(.x)[1] == "sf") {
    # Check if input geometry type is point, if not convert to point geometry type by
    # extracting centroid. This may not be the appropriate behavior?
    if (!inherits(sf::st_geometry(.x), "sfc_POINT")) {
      .x <- sf::st_centroid(.x)
    }
    crs <- sf::st_crs(.x)$epsg
    .xy <- sf::st_coordinates(.x)
    .y <- .xy[,"Y"]
    .x <- .xy[,"X"]
    list(.x = .x, .y = .y)
  }
}

# Todo: check function in rmapshaper, e.g. ms_erase and ms_dissolve
# Note check the erase function rmapshaper::ms_erase

#' Title
#'
#' @param x
#' @param y
#'
#' @returns
#' @export
#'
#' @examples
st_erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(sf::st_combine(y)))
}

#' Title
#'
#' @param x
#' @param y
#'
#' @returns
#' @export
#'
#' @examples
st_union_full <- function(x, y) {

  # function doing a "real" GIS union operation such as in QGIS or ArcGIS
  # source: https://stackoverflow.com/questions/54710574/how-to-do-a-full-union-with-the-r-package-sf
  # x - the first sf
  # y - the second sf

  sf::st_agr(x) = "constant"
  sf::st_agr(y) = "constant"

  op1 <- sf::st_difference(x, sf::st_union(y))
  op2 <- sf::st_difference(y, sf::st_union(x))
  op3 <- sf::st_intersection(y, x)

  # union <- plyr::rbind.fill(op1, op2, op3)
  # return(sf::st_as_sf(union))
  sf::st_as_sf(dplyr::bind_rows(op1, op2, op3))
}

#' Title
#'
#' @param .x
#' @param .y
#' @param .pred
#'
#' @returns
#' @export
#'
#' @examples
st_filter = function(.x, .y, .pred = sf::st_intersects) {
  # this is equal to .x[.y, op = st_intersects]
  # check that dplyr is loaded, then
  dplyr::filter(.x, lengths(.pred(.x, .y)) > 0)
}

# purrr::reduce does not work with sf-objects, need to drop geometry
# https://github.com/r-spatial/sf/issues/798

# Would be good to use dplyr::join_by with {{ .join_columns }}
# rather than by argument, but can not get this to work:
# Error: Expressions must use one of:

#' Title
#'
#' @param .data
#' @param .join_data
#' @param .join_columns
#'
#' @returns
#' @export
#'
#' @examples
st_join_n <- function(
    .data,
    .join_data,
    .join_columns) {

  .x <- purrr::map(
    .join_data,
    \(x) {
      .data |>
        sf::st_join(x) |>
        sf::st_drop_geometry()
    }
  ) |>
    purrr::reduce(
      dplyr::left_join,
      by = .join_columns
    )

  .data |>
    dplyr::left_join(.x, by = .join_columns)
}

#' Title
#'
#' @param .data
#' @param .join_data
#'
#' @returns
#' @export
#'
#' @examples
st_join_n_loop <- function(.data, .join_data) {
  for (i in seq_along(.join_data)) {
    .data <- .data |> sf::st_join(.join_data[[i]])
  }
  .data
}

#' Title
#'
#' @param x
#' @param y
#' @param output
#'
#' @returns
#' @export
#'
#' @examples
st_nearest_feature_with_distance <- function(x, y, output = c("x", "y")) {

  output <- match.arg(output, c("x", "y"))

  # Note that st_join with predicate sf_nearest_feature can
  # be used for this type of operations as well!
  x_inds_nearest <- x |>
    sf::st_nearest_feature(y)

  y_nearest <- dplyr::slice(y, x_inds_nearest)

  d <- sf::st_distance(
    x,
    y_nearest,
    by_element = TRUE
  )

  x_join <- x |>
    sf::st_join(
      y,
      join = sf::st_nearest_feature
    )

  if (output == "x") {
    out <- x_join
  } else {
    out <- y_nearest
  }

  out$distance  <- d
  out
}

#' Title
#'
#' @param x
#' @param y
#'
#' @returns
#' @export
#'
#' @examples
st_nearest_points2 <- function(x, y) {
  sf::st_nearest_points(
    x,
    dplyr::slice(
      y,
      x |>
        sf::st_nearest_feature(y)
    ),
    pairwise = TRUE
  ) |>
    sf::st_sf(geom = _) |>
    dplyr::mutate(
      distance = sf::st_length(geom)
    )
}

#' Title
#'
#' @param polygons
#' @param points
#' @param fn
#' @param name
#'
#' @returns
#' @export
#'
#' @examples
count_points_in_polygons <- function(
    polygons,
    points,
    fn = st_contains,
    name = "n_total"
) {
  polygons[points, ] |>
    {\(.) dplyr::mutate(., {{ name }} := base::lengths(fn(., points)))}()
}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
list_layers <- function(x) {

  # tibble::tibble(
  #   name = x$name,
  #   geomtype = unlist(x$geomtype),
  #   driver = x$driver,
  #   features = x$features,
  #   fields = x$fields
  # )

  with(
    x,
    tibble::tibble(
      name = name,
      geometry_type = geomtype,
      driver,
      features,
      fields,
      crs
    )
  ) |>
    dplyr::mutate(
      geometry_type = purrr::map_chr(geometry_type, \(x) x),
      crs_input = purrr::map_chr(crs, \(x) x$input)
      # crs_wkt = purrr::map_chr(crs, \(x) x$wkt)
    ) |>
    dplyr::select(-crs)
}

#' Title
#'
#' @param .x
#'
#' @returns
#' @export
#'
#' @examples
st_layers_tibble <- function(.x) {
  sf::st_layers(.x) |>
    list_layers()
}

#' Title
#'
#' @param x
#' @param to
#'
#' @returns
#' @export
#'
#' @examples
stdh_cast_substring <- function(x, to = "MULTILINESTRING") {

  ggg <- sf::st_geometry(x)

  if (!unique(sf::st_geometry_type(ggg)) %in% c("POLYGON", "LINESTRING")) {
    stop("Input should be  LINESTRING or POLYGON")
  }
  for (k in 1:length(sf::st_geometry(ggg))) {
    sub <- ggg[k]
    geom <- lapply(
      1:(length(sf::st_coordinates(sub)[, 1]) - 1),
      function(i)
        rbind(
          as.numeric(sf::st_coordinates(sub)[i, 1:2]),
          as.numeric(sf::st_coordinates(sub)[i + 1, 1:2])
        )
    ) %>%
      sf::st_multilinestring() %>%
      sf::st_sfc()

    if (k == 1) {
      endgeom <- geom
    }
    else {
      endgeom <- rbind(endgeom, geom)
    }
  }
  endgeom <- endgeom %>% st_sfc(crs = sf::st_crs(x))
  if (class(x)[1] == "sf") {
    endgeom <- sf::st_set_geometry(x, endgeom)
  }

  if (to == "LINESTRING") {
    endgeom <- endgeom %>% sf::st_cast("LINESTRING")
  }
  return(endgeom)
}

#' Title
#'
#' @param .data
#' @param .crs
#' @param .area_unit
#'
#' @returns
#' @export
#'
#' @examples
split_holes <- function(.data, .crs = 3006, .area_unit = "km^2") {
  # Extract coordinates
  out <- sf::st_coordinates(.data) |>
    tibble::as_tibble() |>
    dplyr::group_by(L1)

  if (dplyr::n_groups(out) > 0) {

    out <- out %>%
      # Split into separate lists
      dplyr::group_split() %>%
      # Convert to sf-object, polygons
      purrr::map(~ dplyr::select(., X, Y) %>%
                   as.matrix() %>%
                   list(.) %>%
                   sf::st_polygon() %>%
                   sf::st_sfc(crs = .crs) %>%
                   sf::st_sf()
      )

    out <- out %>%
      # Bind all polygons together
      dplyr::bind_rows() %>%
      # Calculate area of each polygon
      dplyr::mutate(Area = sf::st_area(.)) %>%
      # Set units
      dplyr::mutate(
        Area = units::set_units(Area, .area_unit, mode = "standard")
      ) %>%
      # Sort in descending order based on area
      dplyr::arrange(dplyr::desc(Area))

    out
  }
}

#' Title
#'
#' @param x
#' @param y
#'
#' @returns
#' @export
#'
#' @examples
get_nearest <- function(x, y) {

  near_ind <- sf::st_nearest_feature(x, y)

  dplyr::bind_cols(
    x |> st_drop_geometry(),
    y[near_ind,] |> st_drop_geometry(),
    tibble::tibble(
      distance = sf::st_distance(
        x,
        y[near_ind,],
        by_element = TRUE
      )
    )
  ) |>
    dplyr::mutate(
      identical = dplyr::case_when(
        distance < units::as_units(1, "m") ~ TRUE,
        TRUE ~ FALSE
      )
    )
}


#' Title
#'
#' @param .data
#' @param .na_values
#'
#' @returns
#' @export
#'
#' @examples
fix_import_shp_dates_NA <- function(.data, .na_values = c(-1)) {
  .data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.Date),
        \(x) dplyr::case_when(
          lubridate::year(x) %in% .na_values ~ NA,
          TRUE ~ x
        )
      )
    )
}
