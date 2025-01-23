

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These are the slice header bytes from the blog:
# https://www.cardinalpeak.com/blog/worlds-smallest-h-264-encoder
#
# These were given without explanation
#
# Slice header is Table 7.3.3 in the H264 spec
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slice_header_reference <- as.raw(c(0x00, 0x00, 0x00, 0x01, 0x05, 0x88, 0x84, 0x21, 0xa0))
slice_footer_reference <- as.raw(0x80)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create header for a 'slice' i.e. a single image
#' 
#' A slice is made up of a slice header, a sequence of macroblocks and 
#' a slice footer
#' 
#' Note: The slice header also includes the first macroblock header which 
#' is just the \code{mb_type} bits.  These bits must \emph{immedicately} follow
#' the end of the slice header itself (with no padding or alignment).  
#' 
#' @param mode 'annexb' or 'avc1'
#' @param size size of slice in bytes. only needed for mode = 'avc'
#' @return raw vector containing a slice header.  The end of the slice header
#'         (i.e. the start of the macroblocks) is byte-aligned.
#' @examples
#' create_slice_header('annexb')
#' @import bitstreamio
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_slice_header <- function(mode, size = NULL) {
  stopifnot(mode %in% c('annexb', 'avc'))
  
  bs  <- bs_open(raw(), 'w')
  
  # H264 spec: Sect 7.3.1 NAL unit structure
  bs_write_bit(bs, F)                 # Forbidden zero bit. f(1)
  bs_write_uint(bs, 0, nbits = 2)     # nal_ref_idc.   u(2)
  bs_write_uint(bs, 5, nbits = 5)     # nal_unit_type. 5 = coded slice (Table 7-1)
  
  # H264 spec: Sect 7.3.3
  bs_write_uint_exp_golomb(bs, 0) # ue(v)  first_mb_in_slice 
  bs_write_uint_exp_golomb(bs, 7) # ue(v)  slice_type. I frames for all slices = 7
  bs_write_uint_exp_golomb(bs, 0) # ue(v)  pic_parameter_set_id. Match the ID in PPS
  
  #    log2_max_frame_num_minus4 = 0  (From SPS.)
  #    log2_max_frame_num        = 4
  #    frame_num is maximum of 4 bits
  bs_write_uint(bs, 0, nbits = 4) # u(v)   frame_num
  
  bs_write_uint_exp_golomb(bs, 0) # IDR pic ID ue(v)
  
  #     log2_max_pic_order_cnt_lsb_minus4 = 0   From SPS.
  #     log2_max_pic_order_cnt_lsb        = 4
  #     pic_order_cnt_lsb is 4 bits
  bs_write_uint(bs, 0, nbits = 4) # u(v)   pic_order_cnt_lsb 
  
  # ref_pic_list_modification() Is empty for this I-only encoding
  bs_write_sint_exp_golomb(bs, 0) # slice_qp_delta ue(v)


  # First macroblock header. See table 7.3.5 
  # Macroblocks are of type  = 25 i.e. I_PCM
  bs_write_uint_exp_golomb(bs, 25)  # ue(v)  mb_type
  bs_align(bs, nbits = 8, value = FALSE)
  
  raw_vec <- bs_close(bs)
  
  
  if (mode == 'annexb') {
    # Prefix with code '0x00000001'
    prefix <- as.raw(c(0x00, 0x00, 0x00, 0x01))
  } else {
    # prefix with size
    prefix <- raw() |>
      set_endian('big') |>
      write_uint32(size)
  }
  
  c(prefix, raw_vec)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a footer for each slice to be included after the macroblocks
#' 
#' Because we are writing uncompressed YCbCr data, the end of the macroblock
#' output will always be byte-aligned.  We can just append a single bit (set to 1)
#' and then byte-align the stream again. Thus endeth the slice.
#' 
#' @return raw vector containing a slice footer to be included after the
#'         macroblocks in a slice
#' @examples
#' create_slice_footer()
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_slice_footer <- function() {
  raw_vec  <- bs_open(raw(), 'w')      |>
    bs_write_uint(x = 1, nbits = 1)    |>  # rbsp_stop_one_bit  u(1)
    bs_align(nbits = 8, value = FALSE) |>
    bs_close()

  raw_vec
}


