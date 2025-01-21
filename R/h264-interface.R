

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Initialise an h264 Annex B encoder
#' 
#' @param vc An h264 \emph{video context} object. Created using \code{mp4_open()}
#' @param fps Frames per second for playback. Default: 25
#' @param con Output filename. A character filename or an open binary connection 
#' @return \code{h264_open()} return a video context (\code{vc}) object. \code{h264_close()}
#'        does not return a value.
#' @examplesIf interactive()
#' vc <- h264_open(tempfile())
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h264_open  <- function(con, fps = 25) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Video Context (vc) is just an environment for keeping stateful information
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vc <- new.env()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Are we responsible for closing this connection?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con_owner <- FALSE
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we got a character string, then treat this is a filename.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(con)) {
    con       <- file(con, "wb")
    con_owner <- TRUE
  } else if (!inherits(con, 'connection')) {
    stop("Output connection not understood")
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Populate the context
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vc$con               <- con
  vc$con_owner         <- con_owner
  
  vc$mode              <- 'annexb'
  vc$fps               <- fps
  
  vc$nframes           <- 0L
  vc$width             <- NULL
  vc$height            <- NULL
  
  vc$sps_header        <- NULL 
  vc$pps_header        <- NULL
  vc$slice_header      <- NULL
  vc$slice_footer      <- create_slice_footer()
  vc$macroblock_header <- create_macroblock_header()
  
  
  class(vc) <- 'vc-annexb'
  vc
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname h264_open
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h264_close <- function(vc) {
  stopifnot(inherits(vc, 'vc-annexb'))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Are we responsible for the connectoin? if so, close it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vc$con_owner) {
    close(vc$con)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clear out 'vc' object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rm(list = names(vc), envir = vc)
  attributes(vc) <- NULL
  
  invisible()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write a single frame into the h264
#' 
#' @inheritParams h264_open
#' @param frame Image data. This may be: a native raster, a 3D numeric array 
#'        of RGB data, a 2D matrix of grey data. For array and matrix input, 
#'        all numeric values must be in the
#'        range [0, 1] with no NAs allowed.
#' @return \code{h264} video context (\code{vc}) is returned invisibly
#' @examplesIf interactive()
#' vc <- h264_open()
#' h264_write(vc, frame)
#' h264_close(vc)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
h264_write <- function(vc, frame) {
  stopifnot(inherits(vc, 'vc-annexb'))
  write_slice(vc, frame)
  invisible(vc)
}


