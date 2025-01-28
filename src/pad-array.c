
#define R_NO_REMAP

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


int round_up(int val) {
  if (val == 0) return 16;
  
  int rem = val % 16;
  if (rem == 0) return val;
  
  return val + 16 - rem;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Pad an array with zeros such that its height and width are multiples
// of the macroblock size (i.e. 16)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP pad_array_(SEXP src_, SEXP dst_) {
  int nprotect = 0;
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Sanity Check source
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!Rf_isArray(src_)) {
    Rf_error("'arr' must be an array");
  }
  
  if (!Rf_isNumeric(src_)) {
    Rf_error("'arr' must be numeric");
  }
  
  SEXP dims_ = Rf_getAttrib(src_, R_DimSymbol);
  if (Rf_length(dims_) != 3 || INTEGER(dims_)[2] < 3) {
    Rf_error("'arr' must be 3d array with at least 3 planes");
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // What are the rounded up dimensions for this array?
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int h_src = INTEGER(dims_)[0];
  int w_src = INTEGER(dims_)[1];
  int h_dst = round_up(h_src);
  int w_dst = round_up(w_src);
  
  if (h_src == h_dst && w_src == w_dst) {
    // No need to resize. Already sized correctly.
    return src_;
  }
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Allocate dst (if none given), otherwise check it for sanity
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (Rf_isNull(dst_)) {
    // Rprintf("Alloc new\n");
    // Allocate new array
    SEXP dst_dims_ = PROTECT(Rf_allocVector(INTSXP, 3)); nprotect++;
    INTEGER(dst_dims_)[0] = h_dst;
    INTEGER(dst_dims_)[1] = w_dst;
    INTEGER(dst_dims_)[2] = 3;
    dst_ = PROTECT(Rf_alloc3DArray(REALSXP, h_dst, w_dst, 3)); nprotect++;
  } else {
    // Rprintf("Using dst\n");
    // Confirm array is of correct type
    if (!Rf_isArray(dst_) || !Rf_isNumeric(dst_)) {
      Rf_error("'dst' must be an array");
    }
    SEXP dst_dims_ = Rf_getAttrib(dst_, R_DimSymbol);
    if (Rf_length(dst_dims_) != 3 || INTEGER(dst_dims_)[2] != 3) {
      Rf_error("'dst' must be 3d array with exactly 3 planes");
    }
    if (INTEGER(dst_dims_)[0] != h_dst || INTEGER(dst_dims_)[1] != w_dst) {
      Rf_error("'dst' array must have dims: [%i, %i, %i]", h_dst, w_dst, 3);
    }
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Clear 'dst'
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  memset(REAL(dst_), 0, h_dst * w_dst * 3 * sizeof(double));
  
  // For each plane in 1:3
  //   For each row in original
  //      memcpy() a width worth of pixels into new
  size_t row_offset = floor(0.5 * (h_dst - h_src));
  size_t col_offset = floor(0.5 * (w_dst - w_src));
  
  for (int plane = 0; plane < 3; ++plane) {
    double *src = REAL(src_) + plane * (h_src * w_src);
    double *dst = REAL(dst_) + plane * (h_dst * w_dst);
    
    for (int row = 0; row < h_src; ++row) {
      memcpy(dst + (row_offset + row) * w_dst + col_offset, src + row * w_src, w_src * sizeof(double));
    }
  }
  
  
  UNPROTECT(nprotect);
  return dst_;
}

