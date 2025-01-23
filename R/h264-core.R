
# nocov start

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a macroblock of YCbCr data from a 16x16 luma segment (and 8x8 chroma)
#' 
#' A macroblock is a sequence of bytes:
#'   - a 16x16 segment of luma array
#'   - an 8x8 segment of Cb
#'   - an 8x8 segment of Cr
#' 
#' All array segments are in row-major ordering.
#' 
#' @param vc video context
#' @param ycbcr image as a list of the 3 components (YCbCr) sampled 4:2:0
#' @param row,col location of the macroblock within the image. For this encoder
#'        all images are required to have dimensions which are multiples of 16
#' @return raw vector for this macroblock
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_macroblock <- function(vc, ycbcr, row, col) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the macroblock data (in row-major order)
  #   16x16 'y' matrix
  #    8x8  'Cb' and 'Cr' matrices
  #
  # Remember that all the components have been transposed already to make
  # row-major extraction easier - so access by col first, then row
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  y  <- ycbcr$y [1:16 + (col - 1L) * 16L, 1:16 + (row - 1L) * 16L ]
  cb <- ycbcr$cb[1: 8 + (col - 1L) *  8 , 1: 8 + (row - 1L) *  8L ]
  cr <- ycbcr$cr[1: 8 + (col - 1L) *  8 , 1: 8 + (row - 1L) *  8L ]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The first macroblock in a slice does not get a macroblock header here.
  # this is because the end of the slice header includes the macroblock header
  # prior to the byte alignment before the macroblocks start
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (! ((row == 1L) && (col == 1L))) {
    c(vc$macroblock_header, y, cb, cr)
  } else {
    c(y, cb, cr)
  }
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a numeric RGB array to macroblocks in R
#' 
#' This is a pretty slow operation in R.
#' 
#' @param vc video context
#' @param frame 3d Numeric RGB array
#' @return raw vector containing macroblocks
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
array_to_macroblocks_r <- function(vc, frame) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert RGB array to a list of YCbCr components (subsampled at 4:2:0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ycbcr <- rgb_to_ycbcr(frame)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # transpose the Y, Cb, Cr components here so I don't have to do 
  # transposition at the macroblock level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ycbcr$y  <- t(ycbcr$y)
  ycbcr$cb <- t(ycbcr$cb)
  ycbcr$cr <- t(ycbcr$cr)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Size of image as multiples of macroblock size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width     <- ncol(frame)
  height    <- nrow(frame)
  width_mb  <- width/16
  height_mb <- height/16
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each macroblock,
  #   create a raw vector containing  c(Y, Y, Y, Y..., Cb, Cb, .... Cr, Cr, ...) 
  # assemble all macroblocks into a single raw vector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mb_loc <- expand.grid(col = seq_len(width_mb), row = seq_len(height_mb))
  macroblocks <- mapply(
    create_macroblock,
    row = mb_loc$row, 
    col = mb_loc$col,
    MoreArgs = list(vc = vc, ycbcr = ycbcr),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ) |> unlist(recursive = FALSE, use.names = FALSE)
  
  macroblocks
}

# nocov end

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write the data for a single slice (i.e. frame) 
#' 
#' @inheritParams h264_open
#' @param frame Image data. This may be: a native raster, a 3D numeric array 
#'        of RGB data, a 2D matrix of grey data. For array and matrix input, 
#'        all numeric values must be in the
#'        range [0, 1] with no NAs allowed.
#' @return Length of data written
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_slice <- function(vc, frame) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check we have an acceptable input of the correct size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width  <- ncol(frame)
  height <- nrow(frame)
  
  if (width %% 16 != 0 || height %% 16 != 0) {
    stop(
      "'frame' dimensions must be multiples of 16.\n", 
      "  Use 'pad_array()' to achieve this for arrays (Warning: this is slow).\n",
      "  For native rasters, blit the image onto a suitably sized canvas.\n",
      "  Resizing images is slow - aim to always generate images at the correct size"
    )
  }
  
  stopifnot(exprs = {
    width  > 0
    height > 0
  })
  
  
  if (!inherits(frame, 'nativeRaster')) {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Promote a matrix to an array
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.matrix(frame) && !inherits(frame, 'nativeRaster')) {
      mat_dim <- dim(frame)
      frame <- c(frame, frame, frame)
      dim(frame) <- c(mat_dim, 3)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Sanity check
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    stopifnot(exprs = {
      is.array(frame)
      length(dim(frame)) == 3
      dim(frame)[3] >= 3
    })
  }
  
  

  
  
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # data assembly for this slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  slice <- raw()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is the first frame:
  #    * Note dimensions
  #    * Initialise headers etc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vc$nframes == 0L) {
    vc$width        <- width
    vc$height       <- height
    vc$pps_header   <- create_pps()
    vc$sps_header   <- create_sps(w = vc$width, h = vc$height)
    vc$slice_header <- NULL
    vc$slice_footer <- create_slice_footer()
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # File header for h264 info
    #  - Annex B just uses the marker 0x00000001
    #  - AVC/mp4 encoding wants the prefix to be the number of bytes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (vc$mode == 'annexb') {
      sps_prefix <- as.raw(c(0x00, 0x00, 0x00, 0x01))
      pps_prefix <- as.raw(c(0x00, 0x00, 0x00, 0x01))
    } else {
      sps_prefix <- raw() |>
        set_endian('big') |>
        write_uint32(length(vc$sps_header))
      pps_prefix <- raw() |>
        set_endian('big') |>
        write_uint32(length(vc$pps_header))
    } 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Pre allocate space for macroblocks
    #   This will prevent allocations/garbage collection when using the 
    #   C routines for converting to macroblocks
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    vc$n_macroblocks <- width/16 * height/16
    vc$macroblocks_size <- 
      width * height * 1.5 +      # Y, Cb and Cr data
      (vc$n_macroblocks - 1) * 2  # 2-byte header for all but the first macroblock
    vc$macroblocks <- raw(vc$macroblocks_size)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Assemble slice header
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    slice <- c(
      slice, 
      sps_prefix, vc$sps_header, 
      pps_prefix, vc$pps_header
    )
  } else {
    # If this is not the first frame in the mp4, then just check that
    # the dimensions match the first frame
    stopifnot(exprs = {
      vc$width  == width
      vc$height == height
    })
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert from RGB to YCbCr, and from a full array of pixels to a 
  # sequence of macroblock data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (FALSE) {
    # Base R
    macroblocks <- array_to_macroblocks_r(vc, frame)
  } else {
    # C code
    if (inherits(frame, 'nativeRaster')) {
      macroblocks <- nr_to_macroblocks_c(vc, frame)
    } else {
      macroblocks <- array_to_macroblocks_c(vc, frame)
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write slice header - which needs the size of the contents 
  #           size(?)  + macroblocks         + macroblocks footer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  slice_size <- 4L     + length(macroblocks) + 2L
  slice_header <- create_slice_header(mode = vc$mode, size = slice_size)

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write all the slice info
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  writeBin(slice          , vc$con)
  writeBin(slice_header   , vc$con)
  writeBin(macroblocks    , vc$con)
  writeBin(vc$slice_footer, vc$con)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Increment the frame counter and return the total bytes written 
  # for this slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vc$nframes <- vc$nframes + 1L
  length(slice) + length(slice_header) + length(macroblocks) + length(vc$slice_footer)
}





