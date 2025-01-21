
test_that("Default PPS matches reference", {
  expect_identical(create_pps(), pps_reference[-(1:4)])
})
