

test_that("slice header works", {
  expect_identical(create_slice_header(mode = 'annexb'), slice_header_reference)
  expect_identical(create_slice_footer(), slice_footer_reference)
  expect_identical(create_macroblock_header(), macroblock_header_reference)
})
