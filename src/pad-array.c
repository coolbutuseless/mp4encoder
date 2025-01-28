
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



SEXP pad_array_(SEXP arr_, SEXP dst_) {
  int nprotect = 0;
  
  if (!Rf_isArray(arr_)) {
    Rf_error("'arr' must be an array");
  }
  
  if (!Rf_isNumeric(arr_)) {
    Rf_error("'arr' must be numeric");
  }
  
  
  
  SEXP dims_ = Rf_getAttrib(arr_, R_DimSymbol);
  if (Rf_length(dims_) != 3 || INTEGER(dims_)[2] < 3) {
    Rf_error("'arr' must be 3d array with at least 3 planes");
  }
  
  int h_new = round_up(INTEGER(dims_)[0]);
  int w_new = round_up(INTEGER(dims_)[1]);
  
  if (Rf_isNull(dst_)) {
    Rprintf("Alloc new\n");
    // Allocate new array
    SEXP dst_dims_ = PROTECT(Rf_allocVector(INTSXP, 3)); nprotect++;
    INTEGER(dst_dims_)[0] = h_new;
    INTEGER(dst_dims_)[1] = w_new;
    INTEGER(dst_dims_)[2] = 3;
    dst_ = PROTECT(Rf_alloc3DArray(REALSXP, h_new, w_new, 3)); nprotect++;
    memset(REAL(dst_), 0, h_new * w_new * 3 * sizeof(double));
  } else {
    Rprintf("Using dst\n");
    // Confirm array is of correct type
    if (!Rf_isArray(dst_) || !Rf_isNumeric(dst_)) {
      Rf_error("'dst' must be an array");
    }
    SEXP dst_dims_ = Rf_getAttrib(dst_, R_DimSymbol);
    if (Rf_length(dst_dims_) != 3 || INTEGER(dst_dims_)[2] != 3) {
      Rf_error("'dst' must be 3d array with exactly 3 planes");
    }
    if (INTEGER(dst_dims_)[0] != h_new || INTEGER(dst_dims_)[1] != w_new) {
      Rf_error("'dst' array must have dims: [%i, %i, %i]", h_new, w_new, 3);
    }
  }
  
  
  
  UNPROTECT(nprotect);
  return dst_;
}

