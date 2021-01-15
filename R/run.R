#' Configures an existing startr project
#'
#' Sources \code{config.R} and \code{functions.R} in turn.
#'
#' @export
run_config <- function() {
  source(here::here('config.R'))
  source(dir_src('functions.R'))
}

#' Runs the processing step on a startr project.
#'
#' Runs the pre-processing step (see \code{upstartr::\link[upstartr:begin_processing]{begin_processing}}
#' for details), then sources \code{process.R}, then runs the post-processing step
#' (see \code{upstartr::\link[upstartr:end_processing]{end_processing}} for details).
#'
#' @param should_process_data Either TRUE, FALSE, or pulled from the environment if set.
#'
#' @export
run_process <- function(should_process_data = TRUE) {
  should_process_data_param <- getOption('startr.should_process_data')

  if (!is.null(should_process_data_param)) {
    should_process_data <- should_process_data_param
  }

  if (should_process_data) {
    begin_processing()
    source(dir_src('process.R'))
    end_processing()
  }
}

#' Runs the pre-processing step on a startr project.
#'
#' The pre-processing step, run as part of \code{upstartr::\link[upstartr:run_process]{run_process}}
#' during the \code{process.R} stage of a startr project, logs all variables
#' currently in the global environment, which will then be removed during the
#' post-processing step to keep the startr environment unpolluted.
#'
#' @param should_clean_processing_variables Either TRUE, FALSE, or pulled from the environment if set.
#'
#' @export
begin_processing <- function(should_clean_processing_variables = TRUE) {
  clean_processing_variables_param <- getOption('startr.should_clean_processing_variables')

  if (!is.null(clean_processing_variables_param)) {
    should_clean_processing_variables <- clean_processing_variables_param
  }

  if (should_clean_processing_variables) {
    assign('curr_env', ls(.GlobalEnv), envir = .GlobalEnv)
  }
}

#' Runs the post-processing step on a startr project.
#'
#' The post-processing step, run as part of \code{upstartr::\link[upstartr:run_process]{run_process}}
#' during the \code{process.R} stage of a startr project, removes all variables
#' saved by \code{upstartr::\link[upstartr:begin_processing]{begin_processing}}
#' and then beeps to announce it's finished.
#'
#' @param should_clean_processing_variables Either TRUE, FALSE, or pulled from the environment if set.
#' @param should_beep Either TRUE, FALSE, or pulled from the environment if set.
#'
#' @export
end_processing <- function(should_clean_processing_variables = TRUE, should_beep = TRUE) {
  clean_processing_variables_param <- getOption('startr.should_clean_processing_variables')
  beep_param <- getOption('startr.should_beep')

  if (!is.null(clean_processing_variables_param)) {
    should_clean_processing_variables <- clean_processing_variables_param
  }

  if (!is.null(beep_param)) {
    should_beep <- beep_param
  }

  if (should_clean_processing_variables) {
    ls(.GlobalEnv) %>%
      setdiff(., curr_env) %>%
      as.character() %>%
      rm(list = ., envir = .GlobalEnv)
  }
  if (should_beep) beepr::beep()
}

#' Runs the analysis step for a startr project.
#'
#' Sources \code{analyze.R}.
#'
#' @export
run_analyze <- function() {
  source(dir_src('analyze.R'))
}

#' Runs the visualization step for a startr project.
#'
#' Sources \code{visualize.R}.
#'
#' @export
run_visualize <- function() {
  source(dir_src('visualize.R'))
}

#' Runs the notebook rendering step for a startr project.
#'
#' Renders an RMarkdown notebook using \code{upstartr::\link[upstartr:render_notebook]{render_notebook}}
#' and then beeps.
#'
#' @param filename The filename for the RMarkdown notebook you want to render.
#' @param should_beep Either TRUE, FALSE, or pulled from the environment if set.
#' @param should_render_notebook Either TRUE, FALSE, or pulled from the environment if set.
#'
#' @export
run_render_notebook <- function(filename = 'notebook.Rmd', should_beep = TRUE, should_render_notebook = TRUE) {
  render_notebook_param <- getOption('startr.should_render_notebook')
  beep_param <- getOption('startr.should_beep')

  if (!is.null(render_notebook_param)) {
    should_render_notebook <- render_notebook_param
  }

  if (!is.null(beep_param)) {
    should_beep <- beep_param
  }

  if (should_render_notebook) render_notebook(dir_reports(filename))
  if (should_beep) beepr::beep()
}

#' Renders out an RMarkdown notebook.
#'
#' Renders an RMarkdown notebook using \code{upstartr::\link[upstartr:render_notebook]{render_notebook}}
#' and then beeps.
#'
#' @param notebook_file The path for the RMarkdown notebook you're rendering.
#' @param output_dir The directory to write the outputs to.
#'
#' @export
render_notebook <- function(notebook_file, output_dir = dir_reports()) {
  rmarkdown::render(
    notebook_file,
    output_dir = output_dir,
    encoding = 'utf-8'
  )
}
