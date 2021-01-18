#' @importFrom magrittr %>%
NULL

determine_casing_fn <- function(case) {
  if (case == 'upper') fn <- stringr::str_to_upper
  if (case == 'lower') fn <- stringr::str_to_lower
  if (case == 'sentence') fn <- stringr::str_to_sentence
  if (case == 'title') fn <- stringr::str_to_title
  return(fn)
}
