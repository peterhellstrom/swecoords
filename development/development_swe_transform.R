colnames_geodetic = c("latitude", "longitude")
crs_to = c(3006, 3847)
colnames_geodetic = c("latitude", "longitude")
colnames_grid_prefix = c("north", "east")
colnames_grid = purrr::map(
  crs_to,
  \(x) str_c(colnames_grid_prefix, x, sep = "_")
)

b <- xy1 |> 
  mutate(
    grid_to_geodetic(
      e, n, crs, colnames_geodetic
    )
  )

b

b |> 
  bind_cols(
    map(
      c(3006, 3847),
      \(x) case_when(
        x != b$crs ~ geodetic_to_grid(b$longitude, b$latitude, x, colnames = str_c(c("nord", "ost"), x, sep = "_")),
        TRUE ~ data.frame(b$n, b$e) |> set_names(str_c(c("nord", "ost"), x, sep = "_"))
      )
    )
  )

z <- function(.data, .crs) {
  .data |> 
    mutate(
      map(
        .crs, 
        \(x) geodetic_to_grid(longitude, latitude, x, colnames = str_c(c("nord", "ost"), x, sep = "_"))
      )
    )
}

b |> 
  z(3006)

b |> z(3847)


map(
  c(3006, 3847),
  \(x) case_when(
    x != b$crs ~ geodetic_to_grid(b$longitude, b$latitude, x, colnames = str_c(c("nord", "ost"), x, sep = "_")),
    TRUE ~ data.frame(b$n, b$e) |> set_names(str_c(c("nord", "ost"), x, sep = "_"))
  )
)

z <- function(.crs_to) {
  
  new_cols <- str_c(c("nord", "ost"), .crs_to, sep = "_")
  
  case_when(
    .crs_to != crs ~ geodetic_to_grid(longitude, latitude, .crs_to, colnames = new_cols),
    TRUE ~ data.frame(b$n, b$e) |> set_names(new_cols)
  )
}

b |> z(3847)

