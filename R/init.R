#' Initialize startr project
#'
#' Used to initialize a startr template for analysis. Will enforce some startr-required
#' standards for analysis (such as removing scientific notation, setting timezones, and
#' writing some project configs to `options`).
#'
#' @param author Name and email of the startr project author
#' @param title Title of the startr project
#' @param scipen Which level of scientific precision to use. (Default: 999)
#' @param timezone The timezone for analysis. (Default: 'America/Toronto')
#' @param should_render_notebook Whether the RMarkdown notebook should be rendered. (Default: FALSE)
#' @param should_process_data Whether startr's process step should be run. (Default: TRUE)
#' @param should_timestamp_output_files Whether write_excel's output files should be timestamped. (Default: FALSE)
#' @param should_clean_processing_variables Whether processing variables should be cleaned
#'   from the environment after processing is complete. (Default: TRUE)
#' @param should_beep Whether startr should beep after tasks like processing or knitting RMarkdown notebooks. (Default: TRUE)
#' @param set_minimal_graphics_theme Whether the minimal graphics theme should be used. (Default: TRUE)
#' @param packages Vector of package names, from CRAN, Github or Bioconductor to be installed.
#'   If using GitHub, package names should be in the format 'user/repo', e.g. 'globeandmail/upstartr'.
#'
#' @return No return value, called for side effects
#'
#' @export
initialize_startr <- function(
    author = 'Firstname Lastname <firstlast@example.com>',
    title = 'startr',
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

    if (is.numeric(scipen)) options(scipen = scipen)
    if (is.character(timezone)) Sys.setenv(TZ = timezone)

    options(
      startr.should_render_notebook = should_render_notebook,
      startr.should_process_data = should_process_data,
      startr.should_timestamp_output_files = should_timestamp_output_files,
      startr.should_clean_processing_variables = should_clean_processing_variables,
      startr.should_beep = should_beep
    )

    options(startr.author = author)
    options(startr.title = title)

    if ('cansim' %in% packages) options(cansim.cache_path = dir_data_cache())
    if ('cancensus' %in% packages) {
      options(
        # CANCENSUS_API should be set in your home directory's
        # .Renviron file, and will get pulled down from there
        cancensus.api_key = Sys.getenv(c('CANCENSUS_API')),
        cancensus.cache_path = dir_data_cache(),
      )
    }

    librarian::shelf(packages, lib = NULL)

    if ('tgamtheme' %in% packages) {
      ggplot2::theme_set(tgamtheme::theme_tgam())
      ggplot2::update_geom_defaults('line', list(size = 1, lineend = 'round', colour = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('path', list(size = 1, lineend = 'round', colour = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('density', list(size = 1, colour = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('point', list(size = 2, colour = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('col', list(fill = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('bar', list(fill = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('ribbon', list(fill = tgamtheme::tgam_cols('burgundy')))
      ggplot2::update_geom_defaults('area', list(fill = tgamtheme::tgam_cols('burgundy')))
    } else if (set_minimal_graphics_theme) {
      ggplot2::theme_set(ggplot2::theme_minimal())
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

    cat(crayon::green(glue::glue('{bquote("\u2713")} Your startr project {crayon::bold(title)} has been initialized!\n\n')))
}
