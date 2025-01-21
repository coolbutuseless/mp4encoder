

macroblock_header_reference <- as.raw(c(0x0d, 0x00 ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a header for each macroblock
#' 
#' This is to be included before each macroblock except for the first one 
#' in a slice.  The \code{slice header} already incorporates the macroblock
#' header for the first one in a slice.
#' 
#' @return raw vector containing a macroblock header to be included before
#'         each macroblock in a slice (other than the first one)
#' @examples
#' create_macroblock_header()
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_macroblock_header <- function() {
  
  bs  <- bitstreamio::bs_open(raw(), 'w')
  
  bitstreamio::bs_write_uint_exp_golomb(bs, 25) # ue(v) mb_type.  25 = I_PCM
  bitstreamio::bs_align(bs, nbits = 8, value = FALSE)
  
  raw_vec <- bitstreamio::bs_close(bs)
  raw_vec
}
