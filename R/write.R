#' Write out an Excel file with minimal configuration
#'
#' Takes a tibble or dataframe variable and saves it out as an Excel file
#' using the variable name as the filename.
#'
#' @param variable A tibble or dataframe object.
#' @param output_dir The directory to save the file out to.
#' @param should_timestamp_output_files Either TRUE, FALSE, or pulled from the environment if set.
#'
#' @return No return value, called for side effects
#'
#' @export
write_excel <- function(variable, output_dir = dir_data_out(), should_timestamp_output_files = FALSE) {
  timestamp_param <- getOption('startr.should_timestamp_output_files')

  if (not.null(timestamp_param)) {
    should_timestamp_output_files <- timestamp_param
  }

  filename <- deparse(substitute(variable))

  if (should_timestamp_output_files) {
    now <- Sys.time()
    filename <- glue::glue('{filename}_{format(now, "%Y%m%d%H%M%S")}')
  }

  openxlsx::write.xlsx(variable, file = here::here(output_dir, glue::glue('{filename}.xlsx')))
}

#' Write out a ggplot2 graphic with minimal configuration
#'
#' Takes a \code{ggplot2} object and writes it to disk via \code{ggplot2::\link[ggplot2:ggsave]{ggsave}} using the
#' variable name as the filename.
#'
#' @param variable A tibble or dataframe object.
#' @param format The desired format for the plot, be it 'png', 'pdf', etc. Accepts formats
#'   you'd pass to \code{ggplot2::\link[ggplot2:ggsave]{ggsave}}'s 'device' parameter.
#' @param output_dir The directory to save the plot out to.
#' @param ... Other settings to pass to ggsave, such as format, width, height or dpi.
#'
#' @return No return value, called for side effects
#'
#' @export
write_plot <- function(variable, format = 'png', output_dir = dir_plots(), ...) {
  filename <- deparse(substitute(variable))

  args <- list(plot = variable, ...)

  args[['file']] <- here::here(output_dir, glue::glue("{filename}.{format}"))

  if ('format' == 'pdf') args[['useDingbats']] <- FALSE

  do.call(ggplot2::ggsave, args)
}

#' Write a shapefile to disk
#'
#' Utility function that wraps \code{sf::\link[sf:st_write]{st_write}}, but first
#' removes a previous version of the shapefile if it exists (by default, \code{sf::\link[sf:st_write]{st_write}}
#' will throw an error.)
#'
#' @param shp A spatial object.
#' @param path The desired filepath for the shapefile.
#' @param ... Other settings to pass to st_write, such as format, width, height or dpi.
#'
#' @return No return value, called for side effects
#'
#' @export
write_shp <- function(shp, path, ...) {
  if (file.exists(path)) {
    file.remove(path)
  }

  sf::st_write(shp, path, ...)
}
