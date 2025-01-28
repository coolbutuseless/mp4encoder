

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pad an array so width and height are a multiple of 16
#' 
#' Note: Resizing an array is slow.  For maximum encoding speed, the user should
#' always aim to generate images with dimensions which are multiples of 16.
#' 
#' @param arr numeric 3d array
#' @param dst dst array of the correct size. Or NULL (the default) which will 
#'        allocate a new array
#' @return either a new 3d array with the correct dimensions, or return the provided
#'         array if it was already correctly sized
#' @examples         
#' arr <- array(1, c(8, 32, 3))         
#' arr <- pad_array(arr)
#' # Height is now padded to 16. Width remains unchanged (already a multiple of 16)
#' dim(arr)         
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pad_array <- function(arr){
  
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



#' @rdname pad_array
#' @export
pad_array2 <- function(arr, dst = NULL) {
  .Call(pad_array_, arr, dst)
}

