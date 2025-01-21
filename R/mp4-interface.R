

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Initialise an mp4 encoder
#' 
#' @param vc An mp4 \emph{video context} object. Created using \code{mp4_open()}
#' @param con Output filename. A character filename or an open binary connection 
#'        (which is seekable)
#' @param fps Frmaes per second. Default: 25
#' @param mdat64 Allow video sequences larger than ~4GB by switching to a 64-bit
#'        length field for the core 'mdat' box.  Default: FALSE.
#' @return \code{mp4_open()} return a Video Coding \code{vc} context object. 
#'        \code{mp4_close()}  does not return a value.
#' @examples
#' vc <- mp4_open(tempfile())
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mp4_open  <- function(con, fps = 25, mdat64 = FALSE) {
  vc <- new.env()
  
  con_owner <- FALSE
  
  if (is.character(con)) {
    con       <- file(con, "wb")
    con_owner <- TRUE
  } else if (!inherits(con, 'connection')) {
    stop("Output connection not understood")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Since the 'mdat' size header for mp4 is only known AFTER writing all the 
  # slices, we need to  be able to rewind the connection to go back and write the size in the 
  # stream.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!isSeekable(con)) {
    stop("When writing 'mp4' files to a connection, the connection *MUST* be seekable.")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Populate the context
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vc$con               <- con
  vc$con_owner         <- con_owner
  
  vc$mode              <- 'avc'
  vc$fps               <- fps
  vc$mdat64            <- isTRUE(mdat64)
  
  vc$nframes           <- 0L
  vc$width             <- NULL
  vc$height            <- NULL
  
  vc$sps_header        <- NULL # won't be known until we get the first frame
  vc$pps_header        <- NULL
  vc$slice_header      <- NULL
  vc$slice_footer      <- create_slice_footer()
  vc$macroblock_header <- create_macroblock_header()
  
  vc$slice_lengths <- integer(0)
  
  class(vc) <- 'vc-avc'
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mp4 header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  header <- create_mp4_header(vc)
  writeBin(header, vc$con)
  
  
  vc
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname mp4_open
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mp4_close <- function(vc) {
  
  stopifnot(inherits(vc, 'vc-avc'))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate number of bytes in 'mdat' i.e. sum of all slices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mdat_size <- sum(vc$slice_lengths) + 8L
  
  if (mdat_size >= 2^32 - 8 && !vc$mdat64) {
    stop("Size of h264 data exceeds 2^32 bytes. Please use mp4_open(..., mdat64 = TRUE)")
  }
  
  if (!vc$mdat64) {
    mdat_bytes <- write_uint32(raw(), mdat_size, endian = 'big')
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Rewind the stream to where the 'mdat' atom is defined
    # Overwrite this location with the calculated size
    # Fast-forward to the end of the stream.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    seek(vc$con, 40, origin = 'start')
    writeBin(mdat_bytes, vc$con)
  } else {
    # 64 mdat has a 16byte header: 0x00000001 'mdat' 8byte-length = 16 bytes
    mdat_size <- sum(vc$slice_lengths) + 8L + 8L
    mdat_bytes <- write_uint64(raw(), mdat_size, endian = 'big')
    seek(vc$con, 40, origin = 'start')
    writeBin(mdat_bytes, vc$con)
  }
  
  seek(vc$con, 0, origin = 'end')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # mp4 footer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  footer <- create_mp4_footer(vc)
  writeBin(footer, vc$con)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Are we responsible for the connection? if so, close it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vc$con_owner) {
    close(vc$con)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clear out 'vc' object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rm(list = names(vc), envir = vc)
  attributes(vc) <- NULL
  
  invisible()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write a single frame into the mp4
#' 
#' @inheritParams mp4_open
#' @inheritParams h264_write
#' @return \code{mp4} video context (\code{vc}) is returned invisibly
#' @examplesIf interactive()
#' vc <- mp4_open()
#' mp4_write(vc, frame)
#' mp4_close(vc)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mp4_write <- function(vc, frame) {
  stopifnot(inherits(vc, 'vc-avc'))
  
  # Track lengths of slices as this is needed for the MP4 'moov' atom
  slice_length <- write_slice(vc, frame)
  vc$slice_lengths <- c(vc$slice_lengths, slice_length)

  invisible(vc)
}

