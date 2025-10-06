utils::globalVariables(c("x", "y", "Sulcus"))

#' Lightweight assets for light example
#' Unzips a tiny assets bundle and returns the directory path.
#' @return A character scalar: path to the unzipped assets directory.
#' @export
get_assets_light <- function() {
  zipfile <- system.file("extdata", "assets_light.zip", package = "sulcimap")
  dir <- tempfile("assets_light")
  utils::unzip(zipfile, exdir = dir)
  inner <- file.path(dir, "assets_light")
  if (dir.exists(inner)) {
    return(inner)
  } else {
    return(dir)
  }
}
