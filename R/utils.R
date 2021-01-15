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

#' Combine CSVs in a directory
#'
#' Given a directory (and, optionally, a pattern to search against),
#' concatenate all CSV files into a single tibble.
#'
#' @param dir Path to the directory to look at for files.
#' @param pattern Pattern to use for detecting files. (Default: '*.csv')
#' @param ... Parameters to pass to \code{readr::\link[readr:read_csv]{read_csv}}.
#'
#' @return A tibble of concatenated CSV data.
#'
#' @export
combine_csvs <- function(dir, pattern = '*.csv', ...) {
  list.files(dir, pattern = pattern, full.names = TRUE) %>%
    purrr::set_names() %>%
    map_dfr(function(x) {
      readr::read_csv(file = x, ...)
    }, .id = 'filename')
}

#' Combine all sheets in an Excel file
#'
#' Reads all sheets in a single Excel file using \code{readxl::\link[readxl:read_excel]{read_excel}}
#' and concatenates them into a single, long tibble.
#'
#' @param filepath Path to the Excel file.
#' @param ... Parameters to pass to \code{readxl::\link[readxl:read_excel]{read_excel}}.
#'
#' @return A tibble of concatenated Excel data.
#'
#' @export
read_all_excel_sheets <- function(filepath, ...) {
  filepath %>%
    readxl::excel_sheets() %>%
    purrr::set_names() %>%
    purrr::map_df(function(x) {
      readxl::read_excel(path = filepath, sheet = x, ...)
    }, .id = 'sheet')
}

#' Combine Excel files in a directory
#'
#' Given a directory (and, optionally, a pattern to search against),
#' concatenate all Excel files into a single tibble.
#'
#' @param dir Path to the directory to look at for files.
#' @param pattern Pattern to use for detecting files. (Default: '*.xls[x]?')
#' @param all_sheets Should this function also concatenate all sheets within each
#'   Excel file into one long tibble? (Default: FALSE)
#' @param ... Parameters to pass to \code{readxl::\link[readxl:read_excel]{read_excel}}.
#'
#' @return A tibble of concatenated Excel data.
#'
#' @export
combine_excels <- function(dir, pattern = '*.xls[x]?', all_sheets = FALSE, ...) {
  read_excel_constructor <- function(x) {
    if (all_sheets) {
      read_all_excel_sheets(path = x, ...)
    } else {
      readxl::read_excel(path = x, ...)
    }
  }

  list.files(dir, pattern = pattern, full.names = TRUE) %>%
    purrr::set_names() %>%
    purrr::map_dfr(read_excel_constructor, .id = 'filename')
}

#' Index values
#'
#' Index numeric vector to first value. By default, the index base will be 0,
#' turning regular values into percentage change. In some cases, you may want to index
#' to a different base, like 100, such as if you're looking at financial data.
#'
#' @param m Numeric vector to index to first value.
#' @param base Base to index against. (Default: 0)
#'
#' @return An indexed version of the vector.
#'
#' @export
index <- function(m, base = 0) {
  if (base != 0) {
    indexed <- (m / dplyr::first(m)) * base
  } else {
    indexed <- (m - dplyr::first(m)) / dplyr::first(m)
  }
  return(indexed)
}

#' Calculate mode
#'
#' Calculates the mode of a given vector.
#'
#' @param x Any kind of vector — numeric, character, logical.
#'
#' @return The mode of that vector.
#'
#' @export
mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#' De-accents strings
#'
#' Replace accented characters with their non-accented versions. Useful when dealing
#' with languages like French, Spanish or Portuguese, where accents can lead to
#' compatibility issues during data analysis.
#'
#' @param x A character vector.
#'
#' @return A character vector of strings without accents.
#'
#' @export
unaccent <- function(x) {
  iconv(x, to = 'ASCII//TRANSLIT')
}

#' Removes non-UTF-8 characters
#'
#' Removes non-UTF-8 characters in a given character vector.
#'
#' @param x A character vector.
#'
#' @return A character vector of strings without non-UTF-8 characters.
#'
#' @export
remove_non_utf8 <- function(x) {
  iconv(x, to = 'UTF-8', sub = '')
}

#' Opposite of %in%
#'
#' Given vectors A and B, returns only the entities from vector A that don't occur in vector B.
#'
#' @export
`%not_in%` <- purrr::negate(`%in%`)

#' Opposite of is.na
#'
#' Given a vector, returns TRUE for all entities that aren't NA.
#'
#' @export
not.na <- purrr::negate(is.na)

#' Simplifies strings for analysis
#'
#' Takes a character vector and "simplifies" it by uppercasing, removing most non-alphabetic
#' (or alphanumeric) characters, removing accents, forcing UTF-8 encoding, removing excess spaces,
#' and optionally removing stop words. Useful in cases where you have two large vector of person
#' or business names you need to compare, but where misspellings may be common.
#'
#' @param x A character vector.
#' @param alpha Should alphabetic characters be included in the cleaned up string? (Default: TRUE)
#' @param digits Should digits be included in the cleaned up string? (Default: FALSE)
#' @param unaccent Should characters be de-accented? (Default: TRUE)
#' @param utf8_only Should characters be UTF-8 only? (Default: TRUE)
#' @param uppercase Should characters be uppercased? (Default: TRUE)
#' @param trim Should strings be trimmed of excess spaces? (Default: TRUE)
#' @param stopwords An optional vector of stop words to be removed.
#'
#' @return A character vector of simplified strings.
#'
#' @examples simplify_string(c('J. Jonah Jameson', 'j jonah jameson', 'j   jonah 123   jameson', 'J Jónah Jameson...'))
#' @examples simplify_string(c('123 Business Inc.', '123 business incorporated', '123 ... Business ... Inc.'), digits = TRUE, stopwords = c('INC', 'INCORPORATED'))
#'
#' @export
simplify_string <- function(
    x,
    alpha = TRUE,
    digits = FALSE,
    unaccent = TRUE,
    utf8_only = TRUE,
    uppercase = TRUE,
    trim = TRUE,
    stopwords = NA
  ) {

    x_temp <- x

    if (unaccent) {
      x_temp <- unaccent(x_temp)
    }

    if (utf8_only) {
      x_temp <- remove_non_utf8(x_temp)
    }

    if (uppercase) {
      x_temp <- stringr::str_to_upper(x_temp)
    }

    if (alpha | digits) {
      re <- '^\\s'
      if (alpha) re <- paste(re, 'a-zA-Z', sep = '')
      if (digits) re <- paste(re, '0-9', sep = '')
      x_temp <- stringr::str_replace_all(x_temp, paste('[', re, ']', sep = ''), '')
    }

    if (!any(is.na(stopwords))) {
      if (uppercase) stopwords <- stringr::str_to_upper(stopwords)
      stopwords_regex <- paste0('\\b', paste(stopwords, collapse = '\\b|\\b'), '\\b')
      x_temp <- stringr::str_replace_all(x_temp, stopwords_regex, '')
    }

    if (trim) {
      x_temp <- stringr::str_squish(x_temp)
    }

    return(x_temp)

  }

#' Cleans up column names by forcing them into tidyverse style
#'
#' Zero-configuration function that takes unwieldy column names and
#' coerces them into tidyverse-styled column names.
#'
#' @param x A vector of column names.
#'
#' @return A character vector of column names.
#'
#' @examples clean_columns(c("Date of Purchase", "Item No.", "description", "", "Transaction at Jane's Counter?", "Auditing - Worth it?"))
#'
#' @export
clean_columns <- function(x) {
  cols <- x %>%
    unaccent(.) %>%
    stringr::str_replace_all(., '[\\s]+', '_') %>%
    stringr::str_replace_all(., '[_]+', '_') %>%
    stringr::str_replace_all(., '[^_a-zA-Z]', '') %>%
    stringr::str_to_lower(.) %>%
    stringr::str_squish(.)

  for (i in 1:length(cols)) {
    if (!as.logical(stringr::str_count(cols[i]))) {
      cols[i] <- glue::glue('column_{i}')
    }
    if (any(cols[1:i - 1] == cols[i])) {
      cols[i] <- glue::glue('{cols[i]}_{i}')
    }
  }

  return(cols)
}

#' Converts a character vector to logicals
#'
#' Takes a character vector and converts it to logicals, optionally using
#' a pattern to match against for truthy and falsy values.
#'
#' @param x A character vector.
#' @param truthy A pattern of case-insensitive truthy values to turn into TRUE.
#' @param falsy A pattern of case-insensitive falsy values to turn into FALSE.
#'
#' @return A logical vector.
#'
#' @examples convert_str_to_logical(c('YES', 'Y', 'No', 'N', 'YES', 'yes', 'no', 'Yes', 'NO', 'Y', 'y'))
#'
#' @export
convert_str_to_logical <- function(x, truthy = 'T|TRUE|Y|YES', falsy = 'F|FALSE|N|NO') {
  x %>%
    stringr::str_to_upper(.) %>%
    stringr::str_squish(.) %>%
    stringr::str_replace_all(., truthy, 'TRUE') %>%
    stringr::str_replace_all(., falsy, 'FALSE') %>%
    as.logical(.)
}
