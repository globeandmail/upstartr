#' Construct an arbitrary path.
#'
#' Convenience function that constructs a path. Wraps \code{here::\link[here:here]{here}}.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_path <- function(...) {
  here::here(...)
}

#' Get path within src directory
#'
#' Constructs a path within startr's main R/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_src <- function(...) {
  dir_path('R', ...)
}

#' Get path within raw data directory.
#'
#' Constructs a path within startr's data/raw/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_data_raw <- function(...) {
  dir_path('data/raw', ...)
}

#' Get path within cached data directory.
#'
#' Constructs a path within startr's data/cache/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_data_cache <- function(...) {
  dir_path('data/cache', ...)
}

#' Get path within processed data directory.
#'
#' Constructs a path within startr's data/processed/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_data_processed <- function(...) {
  dir_path('data/processed', ...)
}

#' Get path within disposable data outputs directory.
#'
#' Constructs a path within startr's data/out/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_data_out <- function(...) {
  dir_path('data/out', ...)
}

#' Get path within reports directory.
#'
#' Constructs a path within startr's reports/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_reports <- function(...) {
  dir_path('reports', ...)
}

#' Get path within plots directory.
#'
#' Constructs a path within startr's plots/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_plots <- function(...) {
  dir_path('plots', ...)
}

#' Get path within scrape directory.
#'
#' Constructs a path within startr's scrape/ directory.
#'
#' @param ... Any number of path strings, passed in the same fashion as \code{here::\link[here:here]{here}}.
#'
#' @return A path string.
#'
#' @export
dir_scrape <- function(...) {
  dir_path('scrape', ...)
}
