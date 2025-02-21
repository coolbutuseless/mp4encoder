

test_that("rgb-to-ycbcr has repeatable results works", {
  
  # Small rgb array
  # Repeatable randomisation
  
  set.seed(1)
  w <- 1 * 16
  h <- 1 * 16
  im <- array(runif(h * w * 3), dim = c(h, w, 3))
  
  ycbcr  <- rgb_to_ycbcr(im)
  
  ycbcr_ref <- list(
    y = structure(as.raw(c(0x41, 0x7b, 0x77, 0xc9, 0x71, 0x81, 0x7a, 0xb1, 0xa8, 0x3b, 0x2e, 0x80, 0x96, 0x56, 0x5e, 0x51, 0x88, 0x7f, 0x5e, 0x5c, 0x77, 0x3c, 0x7b, 0x7c, 0x2e, 0x75, 0x86, 0x68, 0x59, 0x50, 0x69, 0x53, 0x46, 0x3b, 0x7c, 0x56, 0xd0, 0x53, 0x96, 0x91, 0x53, 0x60, 0x5d, 0xb5, 0x8c, 0x51, 0x65, 0x7f, 0x80, 0xc0, 0x4e, 0xc3, 0x4c, 0x65, 0x2c, 0x3f, 0x98, 0xa7, 0x61, 0x75, 0x5d, 0x53, 0xc0, 0x83, 0xa1, 0x53, 0x78, 0xd5, 0x8c, 0xdc, 0x9f, 0xad, 0x58, 0x92, 0xb0, 0x7d, 0x94, 0xa4, 0x5a,  0x84, 0x79, 0x63, 0x8a, 0xb2, 0xcd, 0x40, 0x8e, 0x54, 0x8b, 0x53, 0x60, 0x3c, 0x82, 0xbf, 0x8a, 0x56, 0xab, 0x63, 0xdb, 0x64, 0xb5, 0x3f, 0xa6, 0x98, 0x4b, 0x51, 0x94, 0x54, 0xad, 0xa8, 0x66, 0x9e, 0x89, 0x77, 0x58, 0x69, 0xc3, 0x4c, 0x5f, 0x79, 0x84, 0xb4, 0x52, 0x62, 0x7b, 0x78, 0x97, 0xac, 0x53, 0x5d, 0xb4, 0x85, 0x34, 0xbc, 0x60, 0x5f, 0xbd, 0x8b, 0x75, 0x73, 0x9b, 0x69, 0x5b, 0x36, 0x99, 0x54, 0xa9, 0xca, 0x9a, 0xba, 0x69, 0xa9, 0x2f, 0xb5, 0xc8, 0x4f, 0x9c, 0x8c, 0x90, 0x68, 0x77, 0x7f, 0x93,  0xb7, 0x8b, 0x70, 0x71, 0x3f, 0x7f, 0x6e, 0x48, 0xcc, 0xc3, 0xa7, 0xab, 0x5e, 0x6b, 0x9d, 0x47, 0xbf, 0x97, 0x32, 0x93, 0xb5, 0x62, 0x46, 0x9a, 0x9a, 0x87, 0x81, 0x80, 0xb4, 0x36, 0x57, 0xa9, 0x7d, 0x5e, 0x6f, 0x3e, 0xa4, 0x6f, 0xb1, 0x8a, 0x34, 0x31, 0x73, 0x3f, 0x64, 0x42, 0xc7, 0xc2, 0x3d, 0x86, 0xbc, 0x89, 0xae, 0xa1, 0x83, 0x70, 0x53, 0xa3, 0x54, 0x77, 0x92, 0xb5, 0x5a, 0x90, 0x8e, 0x80, 0x96, 0x4e, 0x1f, 0x9f, 0x54, 0x38, 0xcb, 0x66, 0x5a, 0x6c, 0x7f, 0x69, 0x7b, 0xc4, 0x25, 0x7d, 0x7c, 0x99,  0x79, 0x7e, 0xcf, 0x8b, 0xc0, 0x95, 0x80, 0xae, 0x65)), dim = c(16L, 16L)), 
    cb = structure(as.raw(c(0x64, 0x9f, 0x99, 0x6d, 0x91, 0x9a, 0x63, 0xb8, 0x79, 0xbf, 0x57, 0xa0, 0x5a, 0xb5, 0x44, 0xb6, 0x80, 0xae, 0x64, 0x7f, 0xa1, 0x2d, 0xad, 0x9a, 0x9f, 0x81, 0x63, 0x84, 0x71, 0x83, 0x90, 0x92, 0xcb, 0x97, 0xcf, 0x55, 0x72, 0x9b, 0x5d, 0x9c, 0xad, 0x73, 0x66, 0x6d, 0xa1, 0x71, 0x64, 0x57, 0xbb, 0x5d, 0xca, 0xc1, 0x58, 0x70, 0x70, 0x9a, 0x44, 0x40, 0x6b, 0xa9, 0x47, 0xa4, 0x5d, 0x89)), dim = c(8L, 8L)),      
    cr = structure(as.raw(c(0x86, 0x90, 0x59, 0xc9, 0x75, 0x8a, 0x8b, 0xc1, 0xa7, 0xb5, 0x72, 0x91, 0xd2, 0xc5, 0x7a, 0x45, 0x7e, 0x80, 0x32, 0x4d, 0x82, 0x57, 0xa9, 0xc6, 0x57, 0x6d, 0x70, 0x3d, 0xba, 0x34, 0xa1, 0xdd, 0x73, 0x64, 0x6a, 0xd9, 0x5b, 0xd3, 0x88, 0x6f, 0x62, 0x67, 0xb3, 0x43, 0xa2, 0xbc, 0x86, 0x4b, 0x73, 0x3d, 0x58, 0x91, 0x65, 0x79, 0x84, 0xb7, 0x9b, 0x4b, 0x71, 0x8a, 0x53, 0xa1, 0xaf, 0x81)), dim = c(8L, 8L))
  )
  
  
  expect_identical(ycbcr, ycbcr_ref)
  
})
