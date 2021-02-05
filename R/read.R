#' Combine CSVs in a directory
#'
#' Given a directory (and, optionally, a pattern to search against),
#' concatenate all CSV files into a single tibble.
#'
#' @param dir Path to the directory to look at for files.
#' @param pattern Pattern to use for detecting files. (Default: '*.csv')
#' @param ... Parameters to pass to \code{readr::\link[readr:read_csv]{read_csv}}.
#'
#' @return A tibble of concatenated data from multiple CSV files.
#'
#' @export
combine_csvs <- function(dir, pattern = '*.csv', ...) {
  list.files(dir, pattern = pattern, full.names = TRUE) %>%
    purrr::set_names() %>%
    purrr::map_dfr(function(x) {
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
#' @return A tibble data concatenated from a all sheets in an Excel file.
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
#' @return A tibble of concatenated data from multiple Excel files.
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
