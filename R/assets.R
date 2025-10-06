#' @keywords internal
.sulcimap_cache_dir <- function() {
  # Per-user cache; CRAN-friendly (no writes to install dir)
  tools::R_user_dir("sulcimap", which = "cache")
}

#' Ensure bundled assets are available locally (unzips on first use)
#' @return Path to the directory containing extracted assets
#' @keywords internal
.ensure_sulcimap_assets <- function() {
  cache_root <- .sulcimap_cache_dir()
  out_dir    <- file.path(cache_root, "assets")

  # Fast path: already extracted with some expected content
  if (dir.exists(out_dir)) {
    has_pngs <- length(list.files(out_dir, pattern = "\\.png$", recursive = TRUE)) > 0
    has_names <- length(list.files(out_dir, pattern = "names\\.txt$", recursive = TRUE)) > 0
    if (has_pngs || has_names) return(out_dir)
  }

  # Locate the shipped zip inside the installed package
  zip_path <- system.file("extdata", "assets.zip", package = "sulcimap", mustWork = TRUE)

  # Create cache and unzip
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zipfile = zip_path, exdir = out_dir)

  out_dir
}

#' Path to sulcimap extracted asset directory
#'
#' This will unzip the bundled assets into a per-user cache
#' the first time it is called then return the cache path on subsequent calls
#'
#' @return Character scalar: full path to the root of extracted assets
#' @export
get_sulcimap_assets_dir <- function() {
  .ensure_sulcimap_assets()
}

#' List bundled PNGs (after extraction)
#' @return Character vector of relative PNG paths
#' @export
list_sulci_images <- function() {
  d <- .ensure_sulcimap_assets()
  list.files(d, pattern = "\\.png$", recursive = TRUE)
}

#' Full path to a bundled PNG (after extraction)
#' @param name Relative path inside the assets
#' @return Character scalar: full path
#' @export
get_sulcus_image <- function(name) {
  d <- .ensure_sulcimap_assets()
  p <- file.path(d, name)
  if (!file.exists(p)) stop("Image not found in assets: ", name, call. = FALSE)
  p
}

#' Clear sulcimap cached assets
#' @return Logical TRUE if cache removed or did not exist
#' @export
clear_sulcimap_cache <- function() {
  cache_root <- .sulcimap_cache_dir()
  out_dir    <- file.path(cache_root, "assets")
  if (dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE, force = TRUE)
    !dir.exists(out_dir)
  } else TRUE
}
