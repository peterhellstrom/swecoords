#' Title
#'
#' @param .x
#' @param include_bbox
#'
#' @returns
#' @export
#'
#' @examples
gpkg_contents <- function(.x, include_bbox = TRUE) {

  x <- st_layers_tibble(.x)

  con <- DBI::dbConnect(RSQLite::SQLite(), .x)

  x_contents <- DBI::dbGetQuery(
    con, 'SELECT * FROM gpkg_contents'
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      last_change = readr::parse_datetime(.data$last_change)
    )

  DBI::dbDisconnect(con)

  x <- x |>
    dplyr::left_join(
      x_contents,
      dplyr::join_by(name == table_name)
    ) |>
    dplyr::arrange(.data$name)

  x <- x |>
    dplyr::select(-"driver", -"data_type", -"description", -"identifier")

  if (!include_bbox) {
    x <- x |>
      dplyr::select(-c("min_x":"max_y"))
  }

  x

}

# Edit epsg (SRID) to show correctly, st_write can not write the epsg (SRID) because this is apparently done in GDAL
# and there's not a 1:1 relationship between epsg (SRID) and the proj4string. The proj4string is written correctly to the gpkg,
# but SRID is not.
# Check two tables; gpkg_spatial_ref_sys and gpkg_geometry_columns, the SRSID must exist in gpkg_spatial_ref_sys before gpkg_geometry_columns can be updated
# use sqlite-builtin function to set correct SRID and proj4string:
# Generates error message:
# "UNIQUE constraint failed: gpkg_spatial_ref_sys.srs_id"
# but executes as expected anyway....
# Check EPSG (SRID) & proj4string: both should now be correct, epsg (SRID) should not be NA
# Also try load in QGIS: check BOTH Layer properties from the Browser and Database > DB Manager
# Try to load in ArcGIS

# See more info:
# https://github.com/r-spatial/sf/issues/786

# Check if value already exists?

#' Title
#'
#' @param dsn
#' @param layer
#' @param epsg
#' @param delete_srid
#'
#' @returns
#' @export
#'
#' @examples
set_epsg_gpkg <- function(dsn, layer, epsg = 3006, delete_srid = NULL) {
  gdalUtils::ogrinfo(
    dsn, dialect = "sqlite",
    sql = glue::glue("SELECT gpkgInsertEpsgSRID({epsg})")
  )

  gdalUtils::ogrinfo(
    dsn, dialect = "sqlite",
    sql = glue::glue("UPDATE gpkg_geometry_columns SET srs_id = {epsg} WHERE table_name LIKE '{layer}%';")
  )

  gdalUtils::ogrinfo(
    dsn, dialect = "sqlite",
    sql = glue::glue("UPDATE gpkg_contents SET srs_id = {epsg} WHERE table_name LIKE '{layer}%';")
  )

  if (!is.null(delete_srid)) {
    # Delete SRSID posts created "by" st_write [or rather GDAL]
    gdalUtils::ogrinfo(dsn, dialect = "sqlite", sql = glue::glue("DELETE FROM gpkg_spatial_ref_sys WHERE srs_id LIKE {delete_srid}%;"))
  }
}
