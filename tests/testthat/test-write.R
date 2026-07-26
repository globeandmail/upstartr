test_that('write_plot passes useDingbats = FALSE for pdf format', {
  captured_args <- NULL

  local_mocked_bindings(
    ggsave = function(...) {
      captured_args <<- list(...)
    },
    .package = 'ggplot2'
  )

  test_plot <- 'not a real plot, ggsave is mocked'
  write_plot(test_plot, format = 'pdf')

  expect_true(isFALSE(captured_args[['useDingbats']]))
})

test_that('write_plot does not set useDingbats for non-pdf formats', {
  captured_args <- NULL

  local_mocked_bindings(
    ggsave = function(...) {
      captured_args <<- list(...)
    },
    .package = 'ggplot2'
  )

  test_plot <- 'not a real plot, ggsave is mocked'
  write_plot(test_plot, format = 'png')

  expect_null(captured_args[['useDingbats']])
})

test_that('write_plot uses the variable name as the filename', {
  captured_args <- NULL

  local_mocked_bindings(
    ggsave = function(...) {
      captured_args <<- list(...)
    },
    .package = 'ggplot2'
  )

  my_special_plot <- 'not a real plot, ggsave is mocked'
  write_plot(my_special_plot)

  expect_match(captured_args[['file']], 'my_special_plot\\.png$')
})

test_that('write_excel uses the variable name as the filename', {
  captured_path <- NULL

  local_mocked_bindings(
    write.xlsx = function(x, file, ...) {
      captured_path <<- file
    },
    .package = 'openxlsx'
  )

  my_special_table <- data.frame(a = 1, b = 2)
  write_excel(my_special_table)

  expect_match(captured_path, 'my_special_table\\.xlsx$')
})

test_that('write_excel timestamps filenames when requested', {
  captured_path <- NULL

  local_mocked_bindings(
    write.xlsx = function(x, file, ...) {
      captured_path <<- file
    },
    .package = 'openxlsx'
  )

  my_special_table <- data.frame(a = 1, b = 2)
  write_excel(my_special_table, should_timestamp_output_files = TRUE)

  expect_match(captured_path, 'my_special_table_[0-9]{14}\\.xlsx$')
})
