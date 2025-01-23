# Current version in swecoords-package ----
list_layers <- function (x) {
  tibble::tibble(
    name = x$name, 
    geomtype = unlist(x$geomtype), 
    driver = x$driver, 
    features = x$features, 
    fields = x$fields
  )
}

# New version ----
list_layers <- function(path) {
  
  x <- sf::st_layers(path)
  
  with(
    x, 
    tibble::tibble(
      layer_name = name, 
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
    dplyr::select(-crs) |> 
    dplyr::rename(crs = crs_input)
}

# Examples ----
p_gdb <- Sys.getenv("gdb_raptor_monitoring")

list_layers(p_gdb)

list_layers(p_gdb) |> 
  dplyr::filter(is.na(crs) & features == 0)

swecoords::list_layers(sf::st_layers(p_gdb))

p_gpkg <- Sys.getenv("gpkg_havsorn")
list_layers(p_gpkg)
swecoords::gpkg_contents(p_gpkg)
