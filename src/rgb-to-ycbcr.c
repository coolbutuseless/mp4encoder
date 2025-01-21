
#define R_NO_REMAP

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

// rgb_to_ycbcr_mat <- matrix(c(
//     65.738,  129.057,   25.064,  16, 
//     - 37.945, - 74.494,  112.439, 128, 
//     112.439, - 94.154, - 18.285, 128
// ), 3, 4, byrow = TRUE) |> t()




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Convert a numeric RGB array to macroblocks
//
//   * do YCbCr conversion on each color
//   * reshape to macroblocks
//        NOTE: native rasters are already in row-major ordering.  Therefore,
//              there is a reduced amount of byte-swizzling in comparison 
//              to array input
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP array_to_macroblocks_(SEXP arr_, SEXP raw_vec_) {
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Sanity check
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!Rf_isArray(arr_) || !Rf_isNumeric(arr_)) {
    Rf_error("'array_to_macroblocks_(): Argument must be a numeric array");
  }
  
  SEXP dims_ = Rf_getAttrib(arr_, R_DimSymbol);
  if (Rf_length(dims_) != 3 && Rf_length(dims_) != 4) {
    Rf_error("'array_to_macroblocks_(): Must be a 3D array with at least 3 planes");
  }

  int nprotect = 0;
  int height = INTEGER(dims_)[0];
  int width  = INTEGER(dims_)[1];
  if (height % 16 != 0  ||  width % 16 != 0) {
    Rf_error("'array_to_macroblocks_(): dimensions must be a multiple of 16");
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Create temporary space for calculated color components.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uint8_t *y  = malloc(height   * width   * sizeof(uint8_t));
  uint8_t *cb = malloc(height/2 * width/2 * sizeof(uint8_t));
  uint8_t *cr = malloc(height/2 * width/2 * sizeof(uint8_t));
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Pointers to the start of the 3 planes of the array
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double *r = REAL(arr_) + 0 * height * width;
  double *g = REAL(arr_) + 1 * height * width;
  double *b = REAL(arr_) + 2 * height * width;
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Calc 'Y' component
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (int i = 0; i < height * width; ++i) {
    y[i] = (uint8_t)(65.738 * r[i] +  129.057 * g[i] + 25.064 * b[i] + 16);
  }
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Calc Cb + Cr components
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uint8_t *cbp = cb;
  uint8_t *crp = cr;
  
  for (int col = 0; col < width; col += 2) {
    for (int row = 0; row < height; row += 2) {
      int i = col * height + row;
      *cbp++ = (uint8_t)( -37.945 * r[i] + -74.494 * g[i] + 112.439 * b[i] + 128);
      *crp++ = (uint8_t)( 112.439 * r[i] + -94.154 * g[i] + -18.285 * b[i] + 128);
    }
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // WRite macroblock data into provided "raw_vec_" object
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int width_mb  = width /16;
  int height_mb = height/16;
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Determine total length of macroblock
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int n_macroblocks = width_mb * height_mb;
  int macroblocks_length = 
    width * height     +      // Y in full res
    width/2 * height/2 +      // Cb in half res
    width/2 * height/2 +      // Cr in half res
    (n_macroblocks - 1) * 2;  // macroblock header for all but first
  
  if (Rf_length(raw_vec_) != macroblocks_length) {
    Rf_error("raw_vec mismatch: %i != %i", Rf_length(raw_vec_), macroblocks_length);
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Convert column-major R array data to row-major macroblock data
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uint8_t *mb = RAW(raw_vec_);
  int mb_idx = 0;
  for (int mb_row = 0; mb_row < height_mb; ++mb_row) {
    for (int mb_col = 0; mb_col < width_mb; ++mb_col) {
      
      // Y macroblock
      for (int row = 0; row < 16; ++row) {
        int this_row = mb_row * 16 + row;
        for (int col = 0; col < 16; ++col) {
          int this_col = mb_col * 16 + col;
          *mb++ = y[this_row + this_col * height];
        }
      }
      
      // Cb
      for (int row = 0; row < 8; ++row) {
        int this_row = mb_row * 8 + row;
        for (int col = 0; col < 8; ++col) {
          int this_col = mb_col * 8 + col;
          *mb++ = cb[this_row + this_col * height/2];
        }
      }
      
      // Cr
      for (int row = 0; row < 8; ++row) {
        int this_row = mb_row * 8 + row;
        for (int col = 0; col < 8; ++col) {
          int this_col = mb_col * 8 + col;
          *mb++ = cr[this_row + this_col * height/2];
        }
      }
      
      // Write header for next macroblock
      if (mb_idx < n_macroblocks - 1) {
        *mb++ = 0x0d;
        *mb++ = 0x00;
      }
      
    }
  }
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Tidy and return raw vector
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  free(y);
  free(cb);
  free(cr);
  UNPROTECT(nprotect);
  return raw_vec_;
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Convert a native raster to macroblocks
//
//   * do YCbCr conversion on each color
//   * reshape to macroblocks
//        NOTE: native rasters are already in row-major ordering.  Therefore,
//              there is a reduced amount of byte-swizzling in comparison 
//              to array input
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP nr_to_macroblocks_(SEXP nr_, SEXP raw_vec_) {
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Sanity check
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!Rf_isMatrix(nr_) || !Rf_inherits(nr_, "nativeRaster")) {
    Rf_error("'nr_to_macroblocks_(): Argument must be a nativeRastery");
  }
  
  SEXP dims_ = Rf_getAttrib(nr_, R_DimSymbol);
  
  int nprotect = 0;
  int height = INTEGER(dims_)[0];
  int width  = INTEGER(dims_)[1];
  if (height % 16 != 0  ||  width % 16 != 0) {
    Rf_error("'nr_to_macroblocks_(): dimensions must be a multiple of 16");
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Create temporary space for calculated color components.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uint8_t *y  = malloc(height   * width   * sizeof(uint8_t));
  uint8_t *cb = malloc(height/2 * width/2 * sizeof(uint8_t));
  uint8_t *cr = malloc(height/2 * width/2 * sizeof(uint8_t));
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Color conversion from nativeRaster packed RGB to YCbCr
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uint8_t *p = (uint8_t *)INTEGER(nr_);
  
  for (int i = 0; i < height * width; ++i) {
    y[i] = (uint8_t)(65.738/255 * p[0] +  129.057/255 * p[1] + 25.064/255 * p[2] + 16);
    p += 4;
  }
  
  p = (uint8_t *)INTEGER(nr_);
  uint8_t *cbp = cb;
  uint8_t *crp = cr;
  
  for (int row = 0; row < height; row += 2) {
    for (int col = 0; col < width; col += 2) {
      int i = 4 * (row * width + col);
      *cbp++ = (uint8_t)( -37.945/255 * p[i + 0] + -74.494/255 * p[i + 1] + 112.439/255 * p[i + 2] + 128);
      *crp++ = (uint8_t)( 112.439/255 * p[i + 0] + -94.154/255 * p[i + 1] + -18.285/255 * p[i + 2] + 128);
    }
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Write macroblock data into provided 'raw_vec_' object
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int width_mb  = width /16;
  int height_mb = height/16;
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Determine total size of all macroblock data
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  int n_macroblocks = width_mb * height_mb;
  int macroblocks_length = 
    width * height     +      // Y in full res
    width/2 * height/2 +      // Cb in half res
    width/2 * height/2 +      // Cr in half res
    (n_macroblocks - 1) * 2;  // macroblock header for all but first
  
  if (Rf_length(raw_vec_) != macroblocks_length) {
    Rf_error("raw_vec mismatch: %i != %i", Rf_length(raw_vec_), macroblocks_length);
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // copy the row-major native-raster Ycbcr data into row-major macroblock data
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uint8_t *mb = RAW(raw_vec_);
  int mb_idx = 0;
  for (int mb_row = 0; mb_row < height_mb; ++mb_row) {
    for (int mb_col = 0; mb_col < width_mb; ++mb_col) {
      
      // Y macroblock
      for (int i = 0, offset = (mb_row * width + mb_col) * 16; i < 16; ++i, mb += 16, offset += width) { 
        memcpy(mb, y + offset, 16);
      }
      
      // Cb + Cr
      for (int i = 0, offset = (mb_row * width/2 + mb_col) * 8; i < 8; ++i, mb += 8, offset += width/2) {
        memcpy(mb     , cb + offset, 8);
        memcpy(mb + 64, cr + offset, 8);
      }
      
      // Skip over Cr which has already been written
      mb += 8 * 8;
      
      // Write header for next macroblock
      if (mb_idx < n_macroblocks - 1) {
        *mb++ = 0x0d;
        *mb++ = 0x00;
      }
      
    }
  }
  
  
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Create list and return
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  free(y);
  free(cb);
  free(cr);
  UNPROTECT(nprotect);
  return raw_vec_;
}






