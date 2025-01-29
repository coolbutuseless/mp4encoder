
// #define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP array_to_macroblocks_(SEXP arr_, SEXP raw_vec_);
extern SEXP nr_to_macroblocks_   (SEXP nr_ , SEXP raw_vec_);
extern SEXP pad_array_(SEXP arr_, SEXP hjust_, SEXP vjust_, SEXP fill_, SEXP dst_);
extern SEXP pad_nr_   (SEXP nr_ , SEXP hjust_, SEXP vjust_, SEXP fill_, SEXP dst_);

static const R_CallMethodDef CEntries[] = {
  
  {"array_to_macroblocks_", (DL_FUNC) &array_to_macroblocks_, 2},
  {"nr_to_macroblocks_"   , (DL_FUNC) &nr_to_macroblocks_   , 2},
  
  {"pad_array_", (DL_FUNC) &pad_array_, 5},
  {"pad_nr_"   , (DL_FUNC) &pad_nr_   , 5},
  {NULL , NULL, 0}
};


void R_init_mp4encoder(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}



