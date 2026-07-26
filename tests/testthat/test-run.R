test_that('maybe_beep calls beepr::beep when beepr is installed', {
  skip_if_not_installed('beepr')

  called <- FALSE
  local_mocked_bindings(
    beep = function(...) {
      called <<- TRUE
    },
    .package = 'beepr'
  )

  maybe_beep()

  expect_true(called)
})
