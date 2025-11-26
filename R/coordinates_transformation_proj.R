
#' Title
#'
#' @param .data
#' @param coords
#' @param crs_from
#' @param crs_to
#' @param coords_names
#'
#' @returns
#' @export
#'
#' @examples
proj_trans_add_cols <- function(
    .data,
    coords,
    crs_from,
    crs_to,
    coords_names = NULL
) {

  if (is.null(coords_names)) {
    coords_names <- coords
  }

  out <- map(
    crs_to,
    \(crs_to) {
      PROJ::proj_trans(
        .data[coords] |>
          dplyr::rename_with(
            \(x) c("x", "y")
          ),
        target_crs = crs_to,
        source_crs = crs_from
      ) |>
        tibble::as_tibble() |>
        dplyr::rename_with(
          \(x) stringr::str_c(coords_names, crs_to, sep = "_")
        )
    }
  ) |>
    dplyr::bind_cols()

  dplyr::bind_cols(.data, out)
}

#' Title
#'
#' @param .data
#' @param coords
#' @param crs
#' @param crs_to
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
proj_trans_many_crs <- function(
    .data,
    coords = c("x", "y"),
    crs = "crs",
    crs_to = c(3006, 3847, 4619),
    ...
) {
  .data |>
    dplyr::mutate(
      row_id = dplyr::row_number(),
    ) |>
    dplyr::group_split(
      .data[[crs]]
    ) |>
    purrr::map(
      \(x) proj_trans_add_cols(
        x,
        coords = coords,
        crs_from = base::unique(x[[crs]]),
        crs_to = crs_to,
        ...
      )
    ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(row_id) |>
    dplyr::select(-row_id)
}
