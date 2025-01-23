


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These are the PPS bytes from the blog:
# https://www.cardinalpeak.com/blog/worlds-smallest-h-264-encoder
#
# These were given without explanation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pps_reference <- as.raw(c(0x00, 0x00, 0x00, 0x01, 0x68, 0xce, 0x38, 0x80))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create the header for a Picture Parameter Set (PPS)
#' 
#' This is part of the header for the h264 stream
#' 
#' @return raw bytes of the PPS
#' @import bitstreamio
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pps <- function() {
  
  bs  <- bs_open(raw(), 'w')
  
  # H264 spec: Sect 7.3.1 NAL unit structure
  bs_write_bit(bs, F)                 # Forbidden zero bit. f(1)
  bs_write_uint(bs, 3, nbits = 2)     # nal_ref_idc.   u(2)
  bs_write_uint(bs, 8, nbits = 5)     # nal_unit_type. u(5). 7 = SPS, 8 = PPS (Table 7-1)
  
  # H264 Spec: 7.3.2.2 Picture Parameter Set (PPS) Syntax
  bs_write_uint_exp_golomb(bs, 0) # ue(v) pic_parameter_set_id
  bs_write_uint_exp_golomb(bs, 0) # ue(v) seq_parameter_set_id
  bs_write_uint(bs, 0, nbits = 1) # u(1) entropy_coding_mode_flag
  bs_write_uint(bs, 0, nbits = 1) # u(1) bottom_field_pic_order_in_frame_present_flag 
  bs_write_uint_exp_golomb(bs, 0) # ue(v) num_slice_groups_minus1 
  bs_write_uint_exp_golomb(bs, 0) # ue(v) num_ref_idx_l0_default_active_minus1 
  bs_write_uint_exp_golomb(bs, 0) # ue(v) num_ref_idx_l1_default_active_minus1 
  bs_write_uint(bs, 0, nbits = 1) # u(1) weighted_pred_flag
  bs_write_uint(bs, 0, nbits = 2) # u(2) weighted_bipred_flag
  bs_write_sint_exp_golomb(bs, 0) # pic_init_qp_minus26 1 se(v)
  bs_write_sint_exp_golomb(bs, 0) # pic_init_qs_minus26 1 se(v)
  bs_write_sint_exp_golomb(bs, 0) # chroma_qp_index_offset 1 se(v)
  bs_write_uint(bs, 0, nbits = 1) # deblocking_filter_control_present_flag 1 u(1)
  bs_write_uint(bs, 0, nbits = 1) # constrained_intra_pred_flag 1 u(1)
  bs_write_uint(bs, 0, nbits = 1) # redundant_pic_cnt_present_flag 1 u(1)
  bs_write_uint(bs, 1, nbits = 1) # stop bit
  
  raw_vec <- bs_close(bs)
  raw_vec
}

