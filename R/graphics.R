#' Reorder an x or y axis within facets
#'
#' See \code{tidytext::\link[tidytext:reorder_within]{reorder_within}} for details.
#'
#' @name reorder_within
#' @keywords internal
#' @export
#' @importFrom tidytext reorder_within
#' @return A ggplot2 object for reordering elements within facets, used in combination with
#'   \code{upstartr::\link[upstartr:scale_x_reordered]{scale_x_reordered}} or
#'   \code{upstartr::\link[upstartr:scale_y_reordered]{scale_y_reordered}}
NULL

#' Passed after reorder_within to reorder x-axis along facets
#'
#' See \code{tidytext::\link[tidytext:scale_x_reordered]{scale_x_reordered}} for details.
#'
#' @name scale_x_reordered
#' @keywords internal
#' @export
#' @importFrom tidytext scale_x_reordered
#' @return A scale object to be consumed by ggplot2, used in combination with
#'   \code{upstartr::\link[upstartr:reorder_within]{reorder_within}}.
NULL

#' Passed after reorder_within to reorder x-axis along facets
#'
#' See \code{tidytext::\link[tidytext:scale_y_reordered]{scale_y_reordered}} for details.
#'
#' @name scale_y_reordered
#' @keywords internal
#' @export
#' @importFrom tidytext scale_y_reordered
#' @return A scale object to be consumed by ggplot2, used in combination with
#'   \code{upstartr::\link[upstartr:reorder_within]{reorder_within}}.
NULL

#' Create a continuous x-axis scale using percentages
#'
#' Convenience function to return a scale_x_continuous function using percentage labels.
#'
#' @param ... All your usual continuous x-axis scale parameters.
#'
#' @return A scale object to be consumed by ggplot2.
#'
#' @export
scale_x_percent <- function(...) {
  ggplot2::scale_x_continuous(labels = scales::percent, ...)
}

#' Create a continuous y-axis scale using percentages
#'
#' Convenience function to return a scale_y_continuous function using percentage labels.
#'
#' @param ... All your usual continuous y-axis scale parameters.
#'
#' @return A scale object to be consumed by ggplot2.
#'
#' @export
scale_y_percent <- function(...) {
  ggplot2::scale_y_continuous(labels = scales::percent, ...)
}
