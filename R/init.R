#' Initialize startr project
#'
#' Used to initialize a startr template for analysis. Will enforce some startr-required
#' standards for analysis (such as removing scientific notation, setting timezones, and
#' writing some project configs to `options`).
#'
#' @param scipen Which level of scientific precision to use. (Default: 999)
#' @param timezone The timezone for analysis. (Default: 'America/Toronto')
#' @param should_render_notebook Whether the RMarkdown notebook should berendered. (Default: FALSE)
#' @param should_process_data Whether startr's process step should be run. (Default: TRUE)
#' @param should_timestamp_output_files Whether write_excel's output files should be timestamped. (Default: FALSE)
#' @param should_clean_processing_variables Whether processing variables should be cleaned
#'   from the environment after processing is complete. (Default: TRUE)
#' @param should_beep Whether startr should beep after tasks like processing or knitting RMarkdown notebooks. (Default: TRUE)
#' @param set_minimal_graphics_theme Whether the minimal graphics theme should be used. (Default: TRUE)
#' @param packages Vector of package names, from CRAN, Github or Bioconductor to be installed.
#'   If using GitHub, package names should be in the format 'globeandmail/upstartr'.
#'
#' @export
initialize_startr <- function(
    scipen = 999,
    timezone = 'America/Toronto',
    should_render_notebook = FALSE,
    should_process_data = TRUE,
    should_timestamp_output_files = FALSE,
    should_clean_processing_variables = TRUE,
    should_beep = TRUE,
    set_minimal_graphics_theme = TRUE,
    packages = c()
  ) {

    if (scipen) options(scipen = scipen)
    if (timezone) Sys.setenv(TZ = timezone)

    options(
      startr.should_render_notebook = should_render_notebook,
      startr.should_process_data = should_process_data,
      startr.should_timestamp_output_files = should_timestamp_output_files,
      startr.should_clean_processing_variables = should_clean_processing_variables,
      startr.should_beep = should_beep
    )

    if ('cansim' %in% packages) {
      options(cansim.cache_path = dir_data_cache())
    }

    if ('cancensus' %in% packages) {
      options(
        # CANCENSUS_API should be set in your home directory's
        # .Renviron file, and will get pulled down from there
        cancensus.api_key = Sys.getenv(c('CANCENSUS_API')),
        cancensus.cache_path = dir_data_cache(),
      )
    }

    librarian::shelf(packages, lib = NULL)

    if (set_minimal_graphics_theme) {
      ggthemes::theme_set(theme_minimal())
    }

    knitr::opts_chunk$set(
      eval = TRUE,
      echo = FALSE,
      message = FALSE,
      cache = FALSE,
      warning = FALSE,
      error = FALSE,
      comment = '#',
      tidy = FALSE,
      collapse = TRUE,
      results = 'asis',
      fig.width = 12,
      dpi = 150,
      root.dir = here::here()
    )

}
