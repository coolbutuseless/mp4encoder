

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pad an array or native raster so width and height are multiples of 16
#' 
#' Note: For maximum encoding speed, the user should
#' always aim to generate images with dimensions which are multiples of 16.
#' 
#' @param x array or native raster
#' @param hjust,vjust justification of array within expanded area. Default: (0.5, 0.5)
#'        to center the array
#' @param dst a pre-allocated destiation array or native raster of the correct size. 
#'        Or NULL (the default) which will allocate a new object to hold the result.
#' @param fill background fill color. For arrays, specify a 3-element RGB numeric 
#'        vector with values in the range [0, 1].  For native rasters, specify 
#'        an integer color. Default: -16777216 (black).  See \code{colorfast::col_to_int()}
#' @return either a new 3d array or raster with the correct dimensions, or return the provided
#'         array if it was already correctly sized
#' @examples         
#' arr <- array(1, c(8, 32, 3))         
#' arr <- pad_array(arr)
#' # Height is now padded to 16. Width remains unchanged (already a multiple of 16)
#' dim(arr)         
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pad_array <- function(x, hjust = 0.5, vjust = 0.5, fill = c(0, 0, 0), dst = NULL) {
  .Call(pad_array_, x, hjust, vjust, fill, dst)
}


#' @rdname pad_array
#' @export
pad_nr <- function(x, hjust = 0.5, vjust = 0.5, fill = -16777216, dst = NULL) {
  .Call(pad_nr_, x, hjust, vjust, fill, dst)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Old R version of padding an array. 
# C version (with 'dst' provided) is 70x faster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pad_array_r <- function(arr){
  
  stopifnot(exprs = {
    is.array(arr)
    is.numeric(arr)
    length(dim(arr)) == 3
    dim(arr)[3] >= 3
  })
  
  orig_dims <- dim(arr)
  new_dims <- as.integer(ceiling(orig_dims/16) * 16)
  if (isTRUE(all.equal(new_dims[1:2], orig_dims[1:2]))) {
    # Array is already correctly sized
    return(arr)
  }
  
  # Create new array
  new <- array(0, dim = c(new_dims[[1]], new_dims[[2]], orig_dims[[3]]))
  
  # Offset to centralise
  offset_height <- as.integer(0.5 * (floor(new_dims[[1]] - orig_dims[[1]])))
  offset_width  <- as.integer(0.5 * (floor(new_dims[[2]] - orig_dims[[2]])))
  
  # Copy original array into
  new[seq_len(orig_dims[[1]]) + offset_height, seq_len(orig_dims[[2]]) + offset_width, ] <- arr
  
  new
}


if (FALSE) {
  src <- array(0, c(300, 300, 3))
  src
  pad_array(src, hjust = 1, vjust = 10, fill = c(1, 2, 3)) |> bench::mark()

  library(nara)
  nr  <- nr_new(300, 300, fill = 'hotpink')
  dst <- nr_new(304, 304, fill = 'hotpink')
  nr_circle(nr, 5, 5, 30)
  plot(nr)  
  
  pad_nr(nr, hjust = 1, vjust = 1, fill = colorfast::col_to_int('red'), dst = dst) |> bench::mark()
  
  
}



