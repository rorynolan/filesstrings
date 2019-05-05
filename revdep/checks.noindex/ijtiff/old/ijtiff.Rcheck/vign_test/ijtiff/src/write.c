#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

#include "common.h"

#include <Rinternals.h>
#include <Rversion.h>


SEXP write_tif_C(SEXP image, SEXP where, SEXP sBPS, SEXP sCompr, SEXP sFloats) {
  check_type_sizes();
  SEXP dims, img_list = 0;
  tiff_job_t rj;
  TIFF *tiff;
  FILE *f;
  int bps = asInteger(sBPS), compression = asInteger(sCompr),	img_index = 0;
  int n_img = 1;
  uint32 width, height, planes = 1;
  bool floats = asLogical(sFloats);
  if (TYPEOF(image) == VECSXP) {  // if the image was specified as a list
	  if ((n_img = LENGTH(image)) == 0) {
	    Rf_warning("empty image list, nothing to do");
	    return R_NilValue;
	  }
	  img_list = image;
  }
  if (bps != 8 && bps != 16 && bps != 32)
	  Rf_error("currently bits_per_sample must be 8, 16 or 32");
	const char *fn;
	if (TYPEOF(where) != STRSXP || LENGTH(where) != 1)
	  Rf_error("invalid filename");
	fn = CHAR(STRING_ELT(where, 0));
	f = fopen(fn, "w+b");
	if (!f) Rf_error("unable to create %s", fn);
	rj.f = f;
  tiff = TIFF_Open("wm", &rj);
  if (!tiff) {
	  if (!rj.f) free(rj.data);
	  Rf_error("cannot create TIFF structure");
  }
  while (true) {
	  if (img_list) image = VECTOR_ELT(img_list, img_index++);
	  if (TYPEOF(image) != REALSXP && TYPEOF(image) != INTSXP)
	    Rf_error("image must be a numeric array");
  	dims = Rf_getAttrib(image, R_DimSymbol);
	  if (dims == R_NilValue || TYPEOF(dims) != INTSXP ||
          LENGTH(dims) < 2 || LENGTH(dims) > 3) {
	    Rf_error("image must be an array of two or three dimensions");
	  }
    width = INTEGER(dims)[1];
    height = INTEGER(dims)[0];
    if (LENGTH(dims) == 3) planes = INTEGER(dims)[2];
	  TIFFSetField(tiff, TIFFTAG_IMAGEWIDTH, width);
	  TIFFSetField(tiff, TIFFTAG_IMAGELENGTH, height);
	  TIFFSetField(tiff, TIFFTAG_PLANARCONFIG, 1);
	  TIFFSetField(tiff, TIFFTAG_SOFTWARE,
                "ijtiff package, R " R_MAJOR "." R_MINOR);
	  TIFFSetField(tiff, TIFFTAG_BITSPERSAMPLE, bps);
	  TIFFSetField(tiff, TIFFTAG_SAMPLESPERPIXEL, planes);
	  TIFFSetField(tiff, TIFFTAG_SAMPLEFORMAT,
                 floats ? SAMPLEFORMAT_IEEEFP : SAMPLEFORMAT_UINT);
	  TIFFSetField(tiff, TIFFTAG_ROWSPERSTRIP, height);
	  TIFFSetField(tiff, TIFFTAG_COMPRESSION, compression);
	  TIFFSetField(tiff, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
	  uint32 x, y, pl;
	  tdata_t buf = _TIFFmalloc(width * height * planes * (bps / 8));
	  float *data_float;
	  uint8_t *data8;
	  uint16_t *data16;
	  uint32_t *data32;
	  double *real_arr = REAL(image);
	  if (floats) {
	    data_float = (float*) buf;
	    if (!buf) Rf_error("cannot allocate output image buffer");
	    for (y = 0; y < height; y++) {
	      for (x = 0; x < width; x++) {
	        for (pl = 0; pl < planes; pl++) {
	          data_float[(x + y * width) * planes + pl] =
	            (float) real_arr[y + x * height + pl * width * height];
	        }
	      }
	    }
	  } else {
  	  if (bps == 8) {
  	    data8 = (uint8_t*) buf;
  	    if (!buf) Rf_error("cannot allocate output image buffer");
  		  for (y = 0; y < height; y++) {
  		    for (x = 0; x < width; x++) {
  			    for (pl = 0; pl < planes; pl++) {
  			      data8[(x + y * width) * planes + pl] =
  			        (uint8_t) (real_arr[y + x * height + pl * width * height]);
  			    }
  		    }
  		  }
  	  } else if (bps == 16) {
  	    data16 = (uint16_t*) buf;
  	    if (!buf) Rf_error("cannot allocate output image buffer");
  		  for (y = 0; y < height; y++) {
  		    for (x = 0; x < width; x++) {
  			    for (pl = 0; pl < planes; pl++) {
  			      data16[(x + y * width) * planes + pl] =
  			        (uint16_t) (real_arr[y + x * height +
  			                               pl * width * height]);
  			    }
  		    }
  		  }
  	  } else if (bps == 32) {
  	    data32 = (uint32_t*) buf;
  	    if (!buf) Rf_error("cannot allocate output image buffer");
  		  for (y = 0; y < height; y++) {
  		    for (x = 0; x < width; x++) {
  			    for (pl = 0; pl < planes; pl++) {
  			      data32[(x + y * width) * planes + pl] =
  			        (uint32_t) (real_arr[y + x * height + pl * width * height]);
  			    }
  		    }
  		  }
  	  }
	  }
	  TIFFWriteEncodedStrip(tiff, 0, buf, width * height * planes * (bps / 8));
	  _TIFFfree(buf);
	  if (img_list && img_index < n_img) {
	    TIFFWriteDirectory(tiff);
	  } else {
	    break;
	  }
  }
  TIFFClose(tiff);
  return ScalarInteger(n_img);
}
