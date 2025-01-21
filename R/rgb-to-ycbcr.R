



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RGB to YCbCr matrix
# From wikipedia.
#
# Input for multiplipicatoin should be of the form:
#  Matrix:
#       R  G  B  1
#       r  g  b  1
#       r  g  b  1
#
# The 4th column is used to do the offset 
#  (just like applying transforms to homogenous 3d coordinates!)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgb_to_ycbcr_mat <- matrix(c(
  65.738,  129.057,   25.064,  16, 
  - 37.945, - 74.494,  112.439, 128, 
  112.439, - 94.154, - 18.285, 128
), 3, 4, byrow = TRUE) |> t()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert RGB array to 4:2:0 YCbCr 
#' 
#' @param im Numeric RGB array 
#' @return List of 3 components: Y, Cb, Cr. Each component is a numeric array at the 
#'         specified sub-sampling.
#' @examples
#' im <- array(seq(0, 1, length.out = 72), dim = c(4, 6, 3))
#' rgb_to_ycbcr(im)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgb_to_ycbcr <- function(im) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check RGB array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    is.array(im)
    length(dim(im)) == 3
    dim(im)[3] == 3 || dim(im)[3] == 4
    max(im) <= 1
    min(im) >= 0
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep only RGB channels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # im <- im[, , 1:3]
  odim <- dim(im)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reshape into a matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dim(im) <- c(odim[1] * odim[2], odim[3])
  if (odim[3] == 3) {
    im <- cbind(im, 1)
  } else {
    im[,4] <- 1
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Multiply pixel vales by matrix and add offsets
  # Reshape data back to original dimensions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ycbcr <- im %*% rgb_to_ycbcr_mat
  ycbcr <- as.raw(ycbcr)
  odim[3] <- 3L
  dim(ycbcr) <- odim
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the 3 planes of the numeric array into a list
  # This is because the Cb and Cr planes will be halved in size for 4:2:0 subsampling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ycbcr_lst <- list(
    y  = ycbcr[,,1],
    cb = ycbcr[,,2],
    cr = ycbcr[,,3]
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply the specified sub-sampling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ycbcr_lst$cb <- halve_matrix(ycbcr_lst$cb)
  ycbcr_lst$cr <- halve_matrix(ycbcr_lst$cr)
  
  
  ycbcr_lst
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Halve the dimensions of a matrix
#' 
#' @param mat Numeric matrix. Must have even number of pixels in both dimensions.
#' @return New numeric matrix halve the size of the original
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
halve_matrix <- function(mat) {
  stopifnot(exprs = {
    is.matrix(mat)
    length(dim(mat)) == 2
    dim(mat)[1] %% 2 == 0
    dim(mat)[2] %% 2 == 0
  })
  
  # This is a very simple approach which just discards every second pixel
  # Really should take an average.
  odim <- dim(mat)
  out <- mat[c(T, F), c(T, F)]
  out
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Conversion from RGB to YCbCr in C
#' 
#' @param vc video context
#' @param arr RGB array
#' @return list of Y Cb Cr components at 4:2:0 subsampling
#' @examplesIf interactive()
#' array_to_ycbcr(arr)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
array_to_macroblocks_c <- function(vc, arr) {
  .Call(array_to_macroblocks_, arr, vc$macroblocks)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Conversion from RGB to YCbCr in C
#' 
#' @param vc video context
#' @param nr nativeRaster
#' @return list of Y Cb Cr components at 4:2:0 subsampling
#' @examplesIf interactive()
#' array_to_ycbcr(arr)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr_to_macroblocks_c <- function(vc, nr) {
  .Call(nr_to_macroblocks_, nr, vc$macroblocks)
}
