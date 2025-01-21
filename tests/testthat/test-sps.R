
test_that("SPS matches reference", {
  expect_identical(create_sps(w = 128, h = 96), sps_reference[-(1:4)])
})
