#' Index values
#'
#' Index numeric vector to first value. By default, the index base will be 0,
#' turning regular values into percentage change. In some cases, you may want to index
#' to a different base, like 100, such as if you're looking at financial data.
#'
#' @param m Numeric vector to index to first value.
#' @param base Base to index against. (Default: 0)
#'
#' @return An vector of indexed values.
#'
#' @examples calc_index(c(5, 2, 8, 17, 7, 3, 1, -4))
#' @examples calc_index(c(5, 2, 8, 17, 7, 3, 1, -4), base = 100)
#'
#' @export
calc_index <- function(m, base = 0) {
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
#' @return The mode(s) of that vector.
#'
#' @examples calc_mode(c(1, 1, 2, 3, 4))
#' @examples calc_mode(c('the', 'quick', 'brown', 'fox', 'jumped', 'over', 'the', 'lazy', 'dog'))
#' @examples calc_mode(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
#'
#' @export
calc_mode <- function(x) {
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
#' @param remove.nonconverted Should the function remove unmapped encodings? (Default: FALSE)
#' @param ... Parameters passed to \code{textclean::\link[textclean:replace_non_ascii]{replace_non_ascii}}
#'
#' @return A character vector of strings without accents.
#'
#' @examples unaccent('façile')
#' @examples unaccent('Montréal')
#'
#' @export
unaccent <- function(x, remove.nonconverted = FALSE, ...) {
  textclean::replace_non_ascii(x, remove.nonconverted = remove.nonconverted, ...)
}

#' Removes non-UTF-8 characters
#'
#' Removes non-UTF-8 characters in a given character vector.
#'
#' @param x A character vector.
#'
#' @return A character vector of strings without non-UTF-8 characters.
#'
#' @examples non_utf8 <- 'fa\xE7ile'
#'   Encoding(non_utf8) <- 'latin1'
#'   remove_non_utf8(non_utf8)
#'
#' @export
remove_non_utf8 <- function(x) {
  enc <- Encoding(x)
  final_enc <- 'UTF-8'
  if (all(enc == dplyr::first(enc))) final_enc <- dplyr::first(enc)
  if (any(enc == 'unknown')) final_enc <- 'UTF-8'
  iconv(x, final_enc, to = 'UTF-8', sub = '')
}

#' Opposite of \%in\%
#'
#' Given vectors A and B, returns only the entities from vector A that don't occur in vector B.
#' @param x The vector you want to check.
#' @param table Table in which to do lookups against x.
#'
#' @examples c(1, 2, 3, 4, 5) %not_in% c(4, 5, 6, 7, 8)
#'
#' @return Same form of return as \%in\% — except it will return only elements on the lhs that aren't present on the rhs
#'
#' @export
`%not_in%` <- purrr::negate(`%in%`)

#' Opposite of is.na
#'
#' Given a vector, returns TRUE for all entities that aren't NA.
#'
#' @param x A vector to check for NAs against.
#'
#' @examples not.na(c(1, NA, 2, NA))
#'
#' @return A vector of elements that aren't NA
#'
#' @export
not.na <- purrr::negate(is.na)

#' Opposite of is.null
#'
#' Given a list, returns TRUE for all entities that aren't NULL.
#'
#' @param x A vector to check for NULLs against.
#'
#' @examples not.null(list(1, NULL, 2, NULL))
#'
#' @return Elements that aren't NULL
#'
#' @export
not.null <- purrr::negate(is.null)

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
#' @param case What casing should characters use? Can be one of 'upper', 'lower', 'sentence', 'title',
#'   or 'keep' for the existing casing (Default: 'upper')
#' @param trim Should strings be trimmed of excess spaces? (Default: TRUE)
#' @param stopwords An optional vector of stop words to be removed.
#'
#' @return A character vector of simplified strings.
#'
#' @examples simplify_string(c('J. Jonah Jameson', 'j jonah jameson',
#'   'j   jonah 123   jameson', 'J Jónah Jameson...'))
#' @examples simplify_string(c('123 Business Inc.', '123 business incorporated',
#'   '123 ... Business ... Inc.'), digits = TRUE, stopwords = c('INC', 'INCORPORATED'))
#'
#' @export
simplify_string <- function(
    x,
    alpha = TRUE,
    digits = FALSE,
    unaccent = TRUE,
    utf8_only = TRUE,
    case = 'upper',
    trim = TRUE,
    stopwords = NA
  ) {

    x_temp <- x

    if (utf8_only) {
      x_temp <- remove_non_utf8(x_temp)
    }

    if (unaccent) {
      x_temp <- unaccent(x_temp, remove.nonconverted = FALSE)
    }

    if (is.na(case) | is.null(case) | case == '' | case == 'keep') {
      casing_fn <- NULL
    } else {
      casing_fn <- determine_casing_fn(case)
      x_temp <- casing_fn(x_temp)
    }

    if (alpha | digits) {
      re <- '^\\s'
      if (alpha) re <- paste(re, 'a-zA-Z', sep = '')
      if (digits) re <- paste(re, '0-9', sep = '')
      x_temp <- stringr::str_replace_all(x_temp, paste('[', re, ']', sep = ''), '')
    }

    if (!any(is.na(stopwords))) {
      if (typeof(casing_fn) == 'closure') stopwords <- casing_fn(stopwords)
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
#' @examples clean_columns(c("Date of Purchase", "Item No.", "description", "",
#'   "Transaction at Jane's Counter?", "Auditing - Worth it?"))
#'
#' @export
clean_columns <- function(x) {
  cols <- x %>%
    simplify_string(digits = TRUE, case = 'lower') %>%
    stringr::str_replace_all('[\\s]+', '_')

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
#' a vector of patterns to match against for truthy and falsy values.
#'
#' @param x A character vector.
#' @param truthy A vector of case-insensitive truthy values to turn into TRUE.
#' @param falsy A vector of case-insensitive falsy values to turn into FALSE.
#'
#' @return A logical vector.
#'
#' @examples convert_str_to_logical(c('YES', 'Y', 'No', 'N', 'YES', 'yes', 'no', 'Yes', 'NO', 'Y', 'y'))
#'
#' @export
convert_str_to_logical <- function(x, truthy = c('T', 'TRUE', 'Y', 'YES'), falsy = c('F', 'FALSE', 'N', 'NO')) {
  truthy_vals <- stringr::str_to_upper(truthy)
  falsy_vals <- stringr::str_to_upper(falsy)

  t_regex <- paste0('\\b', paste(truthy_vals, collapse = '\\b|\\b'), '\\b')
  f_regex <- paste0('\\b', paste(falsy_vals, collapse = '\\b|\\b'), '\\b')

  x %>%
    simplify_string() %>%
    stringr::str_replace_all(t_regex, 'TRUE') %>%
    stringr::str_replace_all(f_regex, 'FALSE') %>%
    as.logical()
}
