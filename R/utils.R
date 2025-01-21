

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pad an array so width and height are a multiple of 16
#' 
#' @param arr numeric 3d array (not validated for correctness in this function)
#' @return either a new 3d array with the correct dimenions, or return the provided
#'         array if it was already correctly sized
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pad_array <- function(arr){
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



