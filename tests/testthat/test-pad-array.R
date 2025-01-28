
test_that("pad_array() works", {
  
  arr <- array(1, c(21, 15, 3))
  arr <- pad_array(arr)
  expect_equal(dim(arr), c(32, 16, 3))
  
})


test_that("pad_array() R agrees with C", {
  
  a <- array(88, c(10, 10, 3))
  
  expect_identical(
    pad_array (a),
    pad_array2(a)
  )
})
