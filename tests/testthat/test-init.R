local_initialize_startr_options <- function(.local_envir = parent.frame()) {
  touched <- c(
    'repos', 'scipen',
    'startr.should_render_notebook', 'startr.should_process_data',
    'startr.should_timestamp_output_files', 'startr.should_clean_processing_variables',
    'startr.should_beep', 'startr.author', 'startr.title'
  )
  current <- stats::setNames(lapply(touched, getOption), touched)
  withr::local_options(current, .local_envir = .local_envir)
}

test_that('initialize_startr defaults to the Posit Package Manager binary mirror', {
  local_initialize_startr_options()

  initialize_startr()

  expect_equal(unname(getOption('repos')['CRAN']), 'https://packagemanager.posit.co/cran/latest')
})

test_that('initialize_startr leaves repos untouched when repos = NULL', {
  local_initialize_startr_options()
  options(repos = c(CRAN = 'https://my-organizations-mirror.example.com'))

  initialize_startr(repos = NULL)

  expect_equal(unname(getOption('repos')['CRAN']), 'https://my-organizations-mirror.example.com')
})

test_that('initialize_startr respects a custom repos argument', {
  local_initialize_startr_options()

  initialize_startr(repos = 'https://my-organizations-mirror.example.com')

  expect_equal(unname(getOption('repos')['CRAN']), 'https://my-organizations-mirror.example.com')
})
