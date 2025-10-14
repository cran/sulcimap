# --- internals -------------------------------------------------------

# Detect R CMD check
#' @keywords internal
#' @noRd
.sulcimap_is_check <- function() {
  !interactive() && (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
                       nzchar(Sys.getenv("R_TESTS")))
}

# Cache root:
# - normal users: persistent per-user cache
# - under checks: tempdir()
#' @keywords internal
#' @noRd
.sulcimap_cache_dir <- function() {
  if (.sulcimap_is_check()) {
    return(file.path(tempdir(), "sulcimap-cache"))
  }
  tools::R_user_dir("sulcimap", which = "cache")
}

# Choose bundle to unzip:
#' @keywords internal
#' @noRd
.sulcimap_assets_zip <- function() {
  if (.sulcimap_is_check()) {
    return(system.file("extdata", "assets_light.zip", package = "sulcimap", mustWork = TRUE))
  }
  system.file("extdata", "assets.zip", package = "sulcimap", mustWork = TRUE)
}

# Ensure bundled assets are available locally (unzips on first use)
#' @keywords internal
#' @noRd
.ensure_sulcimap_assets <- function() {
  cache_root <- .sulcimap_cache_dir()
  out_dir    <- file.path(cache_root, "assets")

  # Fast path
  if (dir.exists(out_dir)) {
    has_png   <- length(list.files(out_dir, pattern = "\\.png$", recursive = TRUE)) > 0
    has_names <- length(list.files(out_dir, pattern = "names\\.txt$", recursive = TRUE)) > 0
    if (has_png || has_names) return(out_dir)
  }

  # Locate bundle and extract
  zip_path <- .sulcimap_assets_zip()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ok <- TRUE
  withCallingHandlers(
    utils::unzip(zipfile = zip_path, exdir = out_dir),
    warning = function(w) {
      ok <<- FALSE
      invokeRestart("muffleWarning")
    }
  )
  if (!ok) {
    if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)
    stop("sulcimap: failed to extract assets bundle (zip write error).")
  }

  out_dir
}

# --- public API ------------------------------------------------------

#' Path to sulcimap extracted asset directory
#'
#' Unzips the bundled assets into a cache (persistent for users, temporary under checks)
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
#' @param name Relative path inside the assets (e.g. "leftlat/S.C._left.png")
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
