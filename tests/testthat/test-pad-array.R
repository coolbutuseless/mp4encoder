
test_that("pad_array() works", {
  
  arr <- array(1, c(21, 15, 3))
  
  arr <- pad_array(arr)
  
  expect_equal(dim(arr), c(32, 16, 3))
  
})
