
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
SEXP pad_array_(SEXP src_, SEXP hjust_, SEXP vjust_, SEXP fill_, SEXP dst_) {
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
  
  if (!Rf_isNumeric(fill_) || Rf_length(fill_) != 3) {
    Rf_error("'fill' must a length = 3 numeric vector with values in range [0, 1]");
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Justification
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double hjust = Rf_asReal(hjust_);
  double vjust = Rf_asReal(vjust_);
  
  hjust = hjust < 0 ? 0 : hjust;
  hjust = hjust > 1 ? 1 : hjust;
  vjust = vjust < 0 ? 0 : vjust;
  vjust = vjust > 1 ? 1 : vjust;
  
  
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
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Clear 'dst'
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double *p = NULL;
  double *fill = REAL(fill_);
  p = REAL(dst_) + 0 * h_dst * w_dst; for (int i = 0; i < h_dst * w_dst; ++i) p[i] = fill[0];
  p = REAL(dst_) + 1 * h_dst * w_dst; for (int i = 0; i < h_dst * w_dst; ++i) p[i] = fill[1];
  p = REAL(dst_) + 2 * h_dst * w_dst; for (int i = 0; i < h_dst * w_dst; ++i) p[i] = fill[2];
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // For each plane in 1:3
  //   For each col in original
  //      memcpy() a 'height' worth of pixels into new
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_t row_offset = floor(vjust * (h_dst - h_src));
  size_t col_offset = floor(hjust * (w_dst - w_src));
  
  for (int plane = 0; plane < 3; ++plane) {
    double *src = REAL(src_) + plane * (h_src * w_src);
    double *dst = REAL(dst_) + plane * (h_dst * w_dst);
    
    for (int col = 0; col < w_src; ++col) {
      memcpy(dst + (col_offset + col) * h_dst + row_offset, src + col * h_src, h_src * sizeof(double));
    }
  }
  
  
  UNPROTECT(nprotect);
  return dst_;
}




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Pad a native raster such that its height and width are multiples
// of the macroblock size (i.e. 16)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP pad_nr_(SEXP src_, SEXP hjust_, SEXP vjust_, SEXP fill_, SEXP dst_) {
  int nprotect = 0;
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Sanity Check source
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!Rf_isMatrix(src_)) {
    Rf_error("'nr' must be an matrix");
  }
  
  if (!Rf_isInteger(src_)) {
    Rf_error("'nr' must be an integer matrix");
  }
  
  if (!Rf_inherits(src_, "nativeRaster")) {
    Rf_error("'nr' must a native raster");
  }
  
  SEXP dims_ = Rf_getAttrib(src_, R_DimSymbol);
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Justification
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double hjust = Rf_asReal(hjust_);
  double vjust = Rf_asReal(vjust_);
  
  hjust = hjust < 0 ? 0 : hjust;
  hjust = hjust > 1 ? 1 : hjust;
  vjust = vjust < 0 ? 0 : vjust;
  vjust = vjust > 1 ? 1 : vjust;
  
  
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
    dst_ = PROTECT(Rf_allocMatrix(INTSXP, h_dst, w_dst)); nprotect++;
    SEXP cls_ = PROTECT(Rf_mkString("nativeRaster")); nprotect++;
    Rf_setAttrib(dst_, R_ClassSymbol, cls_);
  } else {
    // Rprintf("Using dst\n");
    // Confirm array is of correct type
    if (!Rf_isMatrix(dst_) || !Rf_isInteger(dst_)) {
      Rf_error("'dst' must be an integer matrix");
    }
    if (!Rf_inherits(dst_, "nativeRaster")) {
      Rf_error("'dst' must a native raster");
    }
    
    SEXP dst_dims_ = Rf_getAttrib(dst_, R_DimSymbol);
    if (INTEGER(dst_dims_)[0] != h_dst || INTEGER(dst_dims_)[1] != w_dst) {
      Rf_error("'dst' array must have dims: [%i, %i]", h_dst, w_dst);
    }
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Clear 'dst'
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int *p = INTEGER(dst_);
  int fill = Rf_asInteger(fill_);
  for (int i = 0; i < h_dst * w_dst; ++i) {
    p[i] = fill;
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //   For each row in original
  //      memcpy() a 'height' worth of pixels into new
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_t row_offset = floor(vjust * (h_dst - h_src));
  size_t col_offset = floor(hjust * (w_dst - w_src));
  
  int *src = INTEGER(src_);
  int *dst = INTEGER(dst_);
  
  for (int row = 0; row < h_src; ++row) {
    memcpy(dst + (row_offset + row) * w_dst + col_offset, src + row * w_src, w_src * sizeof(int));
  }
  
  
  UNPROTECT(nprotect);
  return dst_;
}
