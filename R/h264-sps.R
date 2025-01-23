

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These are the SPS bytes from the blog:
# https://www.cardinalpeak.com/blog/worlds-smallest-h-264-encoder
#
# In H264 spec: Table 7.3.2.1.1 Sequence Parameter Set (SPS) data syntax
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sps_reference <- as.raw(c(0x00, 0x00, 0x00, 0x01, 0x67, 0x42, 0x00, 0x0a, 0xf8, 0x41, 0xa2))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a Sequence Parameter Set (SPS) header
#' 
#' This is part of the header for the h264 stream
#' 
#' @inheritParams create_pps
#' @param w,h width and height of image. Must both be a multiple of 16 
#'        (the macroblock size)
#' @return raw vector with SPS header
#' @import bitstreamio
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_sps <- function(w, h) {
  
  if (w %% 16 != 0 || h %% 16 != 0) {
    stop("'w' and 'h' must be multiples of 16 (the macroblock size)")
  }
  
  mb_width  <- w / 16
  mb_height <- h / 16
  
  
  bs  <- bs_open(raw(), 'w')
  
  # H264 spec: Sect 7.3.1 NAL unit structure
  bs_write_bit(bs, F)               # Forbidden zero bit. f(1)
  bs_write_uint(bs, 3, nbits = 2)   # nal_ref_idc.   u(2)
  bs_write_uint(bs, 7, nbits = 5)   # nal_unit_type. u(5). 7 = SPS, 8 = PPS (Table 7-1)
  
  # H264 spec: Table 7.3.2.1.1 Sequence Parameter Set (SPS) data syntax
  bs_write_uint(bs, 66, nbits = 8)  # profile_idc u(66). Baseline profile
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set0_flag u(1)
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set1_flag u(1)
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set2_flag u(1)
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set3_flag u(1)
  bs_write_uint(bs, 0, nbits = 4)   # reserved_zero_4bits u(4)
  bs_write_uint(bs, 10, nbits = 8)  # level_idx u(8)
  bs_write_uint_exp_golomb(bs, 0)   # seq_parameter_set_id ue(v)
  bs_write_uint_exp_golomb(bs, 0)   # log2_max_frame_num_minus4 ue(v)
  bs_write_uint_exp_golomb(bs, 0)   # pic_order_cnt_type ue(v)
  bs_write_uint_exp_golomb(bs, 0)   # log2_max_pic_order_cnt_lsb_minus4 ue(v) 
  bs_write_uint_exp_golomb(bs, 0)   # num_ref_frames
  bs_write_uint(bs, 0, nbits = 1)   # gaps_in_frame_num_value_allowed_flag  u(1)
  bs_write_uint_exp_golomb(bs, mb_width  - 1)   # pic_width_in_mbs_minus_1 ue(v)
  bs_write_uint_exp_golomb(bs, mb_height - 1)   # pic_height_in_map_units_minus_1 ue(v) 
  bs_write_uint(bs, 1, nbits = 1)   # frame_mbs_only_flag
  bs_write_uint(bs, 0, nbits = 1)   # direct_8x8_inference_flag  u(1)
  bs_write_uint(bs, 0, nbits = 1)   # frame_cropping_flag  u(1)
  bs_write_uint(bs, 0, nbits = 1)   # vui_prameters_present_flag  u(1)
  bs_write_uint(bs, 1, nbits = 1)   # rbsp_stop_one_bit  u(1) 
  bs_align(bs, 8)
  
  
  raw_vec <- bs_close(bs)
  raw_vec
}

