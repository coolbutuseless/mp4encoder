

test_that("mp4 encoding works", {

  library(mp4encoder)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For best results, the array dimensions should be a multiple of 16
  # otherwise padding (with black pixels) will be added to each frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  w <- 112  # 7 * 16
  h <-  80  # 5 * 16
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a Video Coding context
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tst_file <- tempfile()
  vc <- mp4_open(tst_file)
  
  coords <- expand.grid(y = seq(h)/h * 2 * pi - pi, x = seq(w)/w * 2 * pi - pi)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # as each frame is created, write it to the mp4
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (angle in seq(0, 2*pi, length.out = 30))  {
    
    # Create an array 
    arr <- array(0, dim = c(h, w, 3))
    arr[,,1] <- 0.5 * (sin(5 * coords$x + 1 * angle) + 1)
    arr[,,2] <- 0.5 * (cos(3 * sin(coords$y) + 2 * angle) + 1)
    arr[,,3] <- 0.5 * (sin(1.5 * coords$y + cos(coords$x) + 3 * angle) + 1)
    
    # Write the array into the mp4
    mp4_write(vc, arr)
  }
  
  mp4_close(vc)
  
  
  ref_file   <- testthat::test_path("video/test-array.mp4")
  ref_data <- readBin(ref_file, 'raw', n = file.size(ref_file))
  
  tst_data <- readBin(tst_file, 'raw', n = file.size(tst_file))
  
  expect_identical(tst_data, ref_data)
  
  
})
