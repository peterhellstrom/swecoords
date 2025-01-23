# Function to convert coordinates in R
# It uses the web service built by Anders Larsson, Uppsala Universitet
# http://ormbunkar.se/koordinater/
# Note that the XML package is required.
# Written by Gustaf.Granath@ebc.uu.se
# Updated by Peter Hellström, Swedish Museum of Natural History, peter.hellstrom@nrm.se
# Last updated 2016-10-17 (new, more modern rewrite as get_coord2 2023-01-31)

# from_coord_sys, valid options:
# AUTO, WGS84 (4326), RT90 2.5 gon V (3021), SWEREF 99 TM (3006), Lat/Long NAD 27, UTM 33N, RUBIN

# to_coord_sys, valid options:
# WGS84 (4326), RT90 2.5 gon V (3021), SWEREF 99 TM (3006), UTM33N

xml_fun <- function(x){
   y <- XML::xpathSApply(x, './a', XML::xmlAttrs)
   if(length(y) > 0){
      y
   } else {
      XML::xmlValue(x)
   }
}

#' Title
#'
#' @param lat
#' @param lon
#' @param simplify
#' @param from_coord_sys
#' @param to_coord_sys
#' @param encoding
#' @param karta
#'
#' @return
#' @export
#'
#' @examples
get_coord <- function(
    lat, lon, simplify = FALSE,
    from_coord_sys = "AUTO",
    to_coord_sys = "4326",
    encoding = "UTF-8",
    karta = FALSE) {

  if (missing(from_coord_sys)) from_coord_sys <- "AUTO"
  if (!from_coord_sys %in% c("AUTO", "4326", "3021", "3006")) stop("Not a valid input coordinate system")
  if (!to_coord_sys %in% c("4326", "3021", "3006")) stop("Not a valid input coordinate system")

  latlon <- cbind(lat, lon)

  Cclass <- c(
    "numeric","numeric","character","character","character","character",
    "numeric","numeric","character","numeric", "character","character",
    "character","character","character","character","character"
  )

  res <- vector("list", nrow(latlon))

  if (from_coord_sys != "AUTO") {
    from_coord_sys <- paste("EPSG:", from_coord_sys, sep = "")
  }

  for (i in 1:nrow(latlon)) {
    doc <- paste(
      "http://ormbunkar.se/koordinater/GeorefServlet?",
      "lat=", latlon[i,1],"&lon=", latlon[i,2],
      "&from_coord_sys=", from_coord_sys,
      "&to_coord_sys=EPSG:", to_coord_sys, sep = ""
    )

    doc.table <- XML::getNodeSet(XML::htmlParse(doc), "//table")[[1]]

    res[[i]] <- XML::readHTMLTable(
      doc.table,
      colClasses = Cclass,
      stringsAsFactors = FALSE,
      encoding = encoding,
      elFun = xml_fun
    )
  }

  res <- do.call("rbind", res)
  rownames(res) <- NULL

  if (karta == FALSE) res <- res[,-ncol(res)]

  if (simplify == TRUE) {
    short.res <- res[,c(7,8)]
    colnames(short.res) <- c("New_lat","New_lon")
    return(short.res)
  } else {
    return(res)
  }
}

# More modern and "straight-forward" code

#' Title
#'
#' @param lat
#' @param lon
#' @param from_coord_sys
#' @param to_coord_sys
#' @param karta
#'
#' @return
#' @export
#'
#' @examples
get_coord2 <- function(
    lat, lon,
    from_coord_sys = "AUTO",
    to_coord_sys = "4326",
    karta = FALSE) {

  if (from_coord_sys != "AUTO") from_coord_sys <- glue::glue("EPSG:{from_coord_sys}")

  url_str <- glue::glue("http://ormbunkar.se/koordinater/GeorefServlet?lat={lat}&lon={lon}&from_coord_sys={from_coord_sys}&to_coord_sys=EPSG:{to_coord_sys}")

  res <- purrr::map_dfr(
    url_str,
    \(x) rvest::read_html(x) |>
      rvest::html_node("table") |>
      rvest::html_table(header = TRUE, fill = TRUE)
    )

  if(!karta) res |> dplyr::select(-Karta)
  res
}

# Not possible to set column classes (and encoding?) in rvest::html_table,
# see discussion: https://stackoverflow.com/questions/37293830/specifying-column-class-in-html-tablervest

# get_coord(c(59.2, 65), c(17.91, 19), to_coord_sys = 3006)
# get_coord(c(59.2, 65), c(17.91, 19), to_coord_sys = 3006, simplify = TRUE)
# get_coord2(c(59.2, 65), c(17.91, 19), to_coord_sys = 3006)
