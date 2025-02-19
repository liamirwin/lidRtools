# tests/testthat/test-clip_transect_interactive.R

test_that("clip_transect_interactive stops when input is invalid", {
  # This test checks that the function errors out for non-file, non-LAS inputs.
  expect_error(
    clip_transect_interactive(12345),
    "Input is neither a LAS object nor a valid file path."
  )
})

test_that("clip_transect_interactive can handle a non-existent file", {
  # If you pass a nonexistent file path, it should error out:
  expect_error(
    clip_transect_interactive("non_existent_file.laz"),
    "could not be read or is empty"
  )
})

