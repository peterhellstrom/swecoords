#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
get_line_components <- function(.data) {
  out <- .data |>
    sf::st_touches() |>
    igraph::graph_from_adj_list() |>
    igraph::components()

  out$membership
}

#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
get_line_by_group <- function(.data) {
  .data |>
    dplyr::mutate(
      membership = get_line_components(.data[["geom"]])
    ) |>
    dplyr::summarize(
      n_components = n(),
      .by = membership,
      dplyr::across(geom, sf::st_union)
    ) |>
    sf::st_line_merge() |>
    dplyr::mutate(
      length = sf::st_length(geom),
      n_pts = mapview::npts(geom, by_feature = TRUE)
    ) |>
    dplyr::arrange(dplyr::desc(length)) |>
    dplyr::mutate(
      id = dplyr::row_number()
    ) |>
    dplyr::relocate(id, membership, length)
}
