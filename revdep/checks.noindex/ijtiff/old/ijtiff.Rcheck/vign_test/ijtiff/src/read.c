#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#include "common.h"

#include <Rinternals.h>

// avoid protection issues with setAttrib
// where new symbols may trigger GC probelms
static void setAttr(SEXP x, const char *name, SEXP val) {
  PROTECT(val);
  setAttrib(x, Rf_install(name), val);
  UNPROTECT(1);  // UNPROTECT val
}

// Add information attributes according to the TIFF tags.
// Only a somewhat random set (albeit mostly baseline) is supported
static void TIFF_add_tags(TIFF *tiff, SEXP res) {
  uint32_t i32;
  uint16_t i16;
  float f;
  char *c = 0;
  if (TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &i32))
    setAttr(res, "width", ScalarInteger(i32));
  if (TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &i32))
    setAttr(res, "length", ScalarInteger(i32));
  if (TIFFGetField(tiff, TIFFTAG_IMAGEDEPTH, &i32))
    setAttr(res, "depth", ScalarInteger(i32));
  if (TIFFGetField(tiff, TIFFTAG_BITSPERSAMPLE, &i16))
    setAttr(res, "bits_per_sample", ScalarInteger(i16));
  if (TIFFGetField(tiff, TIFFTAG_SAMPLESPERPIXEL, &i16))
    setAttr(res, "samples_per_pixel", ScalarInteger(i16));
  if (TIFFGetField(tiff, TIFFTAG_SAMPLEFORMAT, &i16)) {
    char uv[24];
    const char *name = 0;
    switch (i16) {
    case 1: name = "uint"; break;
    case 2: name = "int"; break;
    case 3: name = "float"; break;
    case 4: name = "undefined"; break;
    case 5: name = "complex int"; break;
    case 6: name = "complex float"; break;
    default:
      snprintf(uv, sizeof(uv), "unknown (%d)", i16);
    name = uv;
    }
    setAttr(res, "sample_format", mkString(name));
  } else {
    setAttr(res, "sample_format", mkString("uint"));
  }
  if (TIFFGetField(tiff, TIFFTAG_PLANARCONFIG, &i16)) {
    if (i16 == PLANARCONFIG_CONTIG)
      setAttr(res, "planar_config", mkString("contiguous"));
    else if (i16 == PLANARCONFIG_SEPARATE)
      setAttr(res, "planar_config", mkString("separate"));
    else {
      char uv[24];
      snprintf(uv, sizeof(uv), "unknown (%d)", i16);
      setAttr(res, "planar_config", mkString(uv));
    }
  }
  if (TIFFGetField(tiff, TIFFTAG_ROWSPERSTRIP, &i32))
    setAttr(res, "rows_per_strip", ScalarInteger(i32));
  if (TIFFGetField(tiff, TIFFTAG_TILEWIDTH, &i32)) {
    setAttr(res, "tile_width", ScalarInteger(i32));
    TIFFGetField(tiff, TIFFTAG_TILELENGTH, &i32);
    setAttr(res, "tile_length", ScalarInteger(i32));
  }
  if (TIFFGetField(tiff, TIFFTAG_COMPRESSION, &i16)) {
    char uv[24];
    const char *name = 0;
    switch (i16) {
    case 1: name = "none"; break;
    case 2: name = "CCITT RLE"; break;
    case 32773: name = "PackBits"; break;
    case 3: name = "CCITT Group 3 fax"; break;
    case 4: name = "CCITT Group 4 fax"; break;
    case 5: name = "LZW"; break;
    case 6: name = "old JPEG"; break;
    case 7: name = "JPEG"; break;
    case 8: name = "deflate"; break;
    case 9: name = "JBIG b/w"; break;
    case 10: name = "JBIG color"; break;
    default:
      snprintf(uv, sizeof(uv), "unknown (%d)", i16);
    name = uv;
    }
    setAttr(res, "compression", mkString(name));
  }
  if (TIFFGetField(tiff, TIFFTAG_THRESHHOLDING, &i16))
    setAttr(res, "threshholding", ScalarInteger(i16));
  if (TIFFGetField(tiff, TIFFTAG_XRESOLUTION, &f))
    setAttr(res, "x_resolution", ScalarReal(f));
  if (TIFFGetField(tiff, TIFFTAG_YRESOLUTION, &f))
    setAttr(res, "y_resolution", ScalarReal(f));
  if (TIFFGetField(tiff, TIFFTAG_XPOSITION, &f))
    setAttr(res, "x_position", ScalarReal(f));
  if (TIFFGetField(tiff, TIFFTAG_YPOSITION, &f))
    setAttr(res, "y_position", ScalarReal(f));
  if (TIFFGetField(tiff, TIFFTAG_RESOLUTIONUNIT, &i16)) {
    const char *name = "unknown";
    switch (i16) {
    case 1: name = "none"; break;
    case 2: name = "inch"; break;
    case 3: name = "cm"; break;
    }
    setAttr(res, "resolution_unit", mkString(name));
  }
#ifdef TIFFTAG_INDEXED /* very recent in libtiff even though it's an old tag */
  if (TIFFGetField(tiff, TIFFTAG_INDEXED, &i16))
    setAttr(res, "indexed", ScalarLogical(i16));
#endif
  if (TIFFGetField(tiff, TIFFTAG_ORIENTATION, &i16)) {
    const char *name = "<invalid>";
    switch (i16) {
    case 1: name = "top_left"; break;
    case 2: name = "top_right"; break;
    case 3: name = "bottom_right"; break;
    case 4: name = "bottom_left"; break;
    case 5: name = "left_top"; break;
    case 6: name = "right_top"; break;
    case 7: name = "right_bottom"; break;
    case 8: name = "left_bottom"; break;
    }
    setAttr(res, "orientation", mkString(name));
  }
  if (TIFFGetField(tiff, TIFFTAG_COPYRIGHT, &c) && c)
    setAttr(res, "copyright", mkString(c));
  if (TIFFGetField(tiff, TIFFTAG_ARTIST, &c) && c)
    setAttr(res, "artist", mkString(c));
  if (TIFFGetField(tiff, TIFFTAG_DOCUMENTNAME, &c) && c)
    setAttr(res, "document_name", mkString(c));
  if (TIFFGetField(tiff, TIFFTAG_DATETIME, &c) && c)
    setAttr(res, "date_time", mkString(c));
  if (TIFFGetField(tiff, TIFFTAG_IMAGEDESCRIPTION, &c) && c)
    setAttr(res, "description", mkString(c));
  if (TIFFGetField(tiff, TIFFTAG_SOFTWARE, &c) && c)
    setAttr(res, "software", mkString(c));
  if (TIFFGetField(tiff, TIFFTAG_PHOTOMETRIC, &i16)) {
    char uv[24];
    const char *name = 0;
    switch (i16) {
    case 0: name = "white is zero"; break;
    case 1: name = "black is zero"; break;
    case 2: name = "RGB"; break;
    case 3: name = "palette"; break;
    case 4: name = "mask"; break;
    case 5: name = "separated"; break;
    case 6: name = "YCbCr"; break;
    case 8: name = "CIELAB"; break;
    case 9: name = "ICCLab"; break;
    case 10: name = "ITULab"; break;
    default:
      snprintf(uv, sizeof(uv), "unknown (%d)", i16);
    name = uv;
    }
    setAttr(res, "color_space", mkString(name));
  }
}

SEXP read_tif_C(SEXP sFn /*filename*/) {
  check_type_sizes();
  uint64_t to_unprotect = 0;
  SEXP res = PROTECT(R_NilValue), multi_res = PROTECT(R_NilValue);
  to_unprotect += 2;  // res and multi_res
  SEXP multi_tail = multi_res, dim;
  const char *fn;  // file name
  uint64_t n_img = 0;
  tiff_job_t rj;
  TIFF *tiff;
  FILE *f;
	if (TYPEOF(sFn) != STRSXP || LENGTH(sFn) != 1) Rf_error("invalid filename");
	fn = CHAR(STRING_ELT(sFn, 0));
	f = fopen(fn, "rb");
	if (!f) Rf_error("unable to open %s", fn);
	rj.f = f;
  tiff = TIFF_Open("rmc", &rj); /* no mmap, no chopping */
  if (!tiff) {
    TIFFClose(tiff);
    Rf_error("Unable to open TIFF");
  }
  while (true) { /* loop over separate image in a directory if desired */
  	uint32_t imageWidth = 0, imageLength = 0, imageDepth;
  	uint32_t tileWidth, tileLength;
  	uint32_t x, y;
  	uint16_t config, bps = 8, spp = 1, sformat = 1, out_spp;
  	tdata_t buf;
  	double *real_arr;
  	uint16_t *colormap[3] = {0, 0, 0};
  	bool is_float = false;
  	TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &imageWidth);
  	TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &imageLength);
  	if (!TIFFGetField(tiff, TIFFTAG_IMAGEDEPTH, &imageDepth)) imageDepth = 0;
  	if (TIFFGetField(tiff, TIFFTAG_TILEWIDTH, &tileWidth)) {
  	  TIFFGetField(tiff, TIFFTAG_TILELENGTH, &tileLength);
  	}	else {  // no tiles
  	  tileWidth = tileLength = 0;
  	}
  	TIFFGetField(tiff, TIFFTAG_PLANARCONFIG, &config);
  	TIFFGetField(tiff, TIFFTAG_BITSPERSAMPLE, &bps);
  	TIFFGetField(tiff, TIFFTAG_SAMPLESPERPIXEL, &spp);
  	out_spp = spp;
  	TIFFGetField(tiff, TIFFTAG_COLORMAP, colormap, colormap + 1, colormap + 2);
  	if (TIFFGetField(tiff, TIFFTAG_SAMPLEFORMAT, &sformat) &&
       sformat == SAMPLEFORMAT_IEEEFP) {
  	  is_float = true;
  	}
  	if (spp == 1) { /* modify out_spp for colormaps */
  	  if (colormap[2]) {
  	    out_spp = 3;
  	  } else if (colormap[1]) {
  	    out_spp = 2;
  	  }
  	}
    #if TIFF_DEBUG
  	  Rprintf("image %d x %d x %d, tiles %d x %d, bps = %d, spp = %d (output %d), "
              "config = %d, colormap = %s\n",
              imageWidth, imageLength, imageDepth, tileWidth, tileLength, bps, spp,
              out_spp, config, colormap[0] ? "yes" : "no");
    #endif
    if (bps == 12) {
      TIFFClose(tiff);
      Rf_error("12-bit images are not supported. "
               "Try converting your image to 16-bit.");
    }
  	if (bps != 8 && bps != 16 && bps != 32) {
  	    TIFFClose(tiff);
  	    Rf_error("image has %d bits/sample which is unsupported", bps);
  	}
  	if (sformat == SAMPLEFORMAT_INT)
  	    Rf_warning("The \'ijtiff\' package only supports unsigned "
                   "integer or float sample formats, but your image contains "
                   "the signed integer format.");
  	res = PROTECT(allocVector(REALSXP, imageWidth * imageLength * out_spp));
  	to_unprotect++;  // res needs to be UNPROTECTed later
  	real_arr = REAL(res);
  	if (tileWidth == 0) {
  	  tstrip_t strip;
  	  tsize_t plane_offset = 0;
  	  x = 0; y = 0;
  	  buf = _TIFFmalloc(TIFFStripSize(tiff));
      #if TIFF_DEBUG
  	    Rprintf(" - %d x %d strips\n",
                TIFFNumberOfStrips(tiff), TIFFStripSize(tiff));
      #endif
    	for (strip = 0; strip < TIFFNumberOfStrips(tiff); strip++) {
    	  tsize_t n = TIFFReadEncodedStrip(tiff, strip, buf, (tsize_t) -1);
    	  if (spp == 1) { // config doesn't matter for spp == 1
    	    if (colormap[0]) {
    	  	  tsize_t i, step = bps / 8;
    			  for (i = 0; i < n; i += step) {
    			    uint32_t ci = 0;
    			    const uint8_t *v = (const uint8_t*) buf + i;
    			    if (bps == 8) {
    			      ci = v[0];
    			    } else if (bps == 16) {
    			      ci = ((const uint16_t*) v)[0];
    			    } else if (bps == 32) {
    			      ci = ((const uint32_t*) v)[0];
    			    }
    			    if (is_float) {
    			      real_arr[imageLength * x + y] = (double) colormap[0][ci];
    			      // color maps are always 16-bit
    			      if (colormap[1]) {
    			        real_arr[(imageLength * imageWidth) + imageLength * x + y] =
    			          (double) colormap[1][ci];
    			        if (colormap[2]) {
    			          real_arr[(2 * imageLength * imageWidth) +
    			                     imageLength * x + y] = (double) colormap[2][ci];
    			        }
    			      }
    			    } else {
    				    real_arr[imageLength * x + y] = colormap[0][ci];
    			      // color maps are always 16-bit
    				    if (colormap[1]) {
    				      real_arr[(imageLength * imageWidth) + imageLength * x + y] =
    				        colormap[1][ci];
    				      if (colormap[2]) {
    					      real_arr[(2 * imageLength * imageWidth) +
    					                imageLength * x + y] =
    					        colormap[2][ci];
    				      }
    				    }
    			    }
    			    x++;
    			    if (x >= imageWidth) {
    				    x -= imageWidth;
    				    y++;
    			    }
    			  }
    		  } else { // direct gray
      			tsize_t i, step = bps / 8;
      			for (i = 0; i < n; i += step) {
      			  const uint8_t *v = (const uint8_t*) buf + i;
      			  if (is_float) {
      			    float float_val = NA_REAL;
      			    float_val = ((const float*) v)[0];
      			    real_arr[imageLength * x + y] = float_val;
      			  } else {
      			    int int_val = NA_INTEGER;
      			    if (bps == 8) {
      			      int_val = v[0];
      			    } else if (bps == 16) {
      			      int_val = ((const uint16_t*)v)[0];
      			    } else if (bps == 32) {
      			      int_val = ((const uint32_t*) v)[0];
      			    }
      			    real_arr[imageLength * x + y] = int_val;
      			  }
      				x++;
      			  if (x >= imageWidth) {
      				  x -= imageWidth;
      				  y++;
      			  }
      			}
    		  }
    		} else if (config == PLANARCONFIG_CONTIG) { // interlaced
    		  tsize_t i, j, step = spp * bps / 8;
    		  for (i = 0; i < n; i += step) {
    			  const uint8_t *v = (const uint8_t*) buf + i;
    			  if (bps == 8) {
    			    for (j = 0; j < spp; j++) {
       				  real_arr[(imageLength * imageWidth * j) + imageLength * x + y] =
       				    (uint8_t) v[j];
    			    }
    			  } else if (bps == 16) {
      			  for (j = 0; j < spp; j++) {
    	  			  real_arr[(imageLength * imageWidth * j) + imageLength * x + y] =
    		  		    (uint16_t) ((const uint16_t*)v)[j];
    			    }
      			} else if (bps == 32 && !is_float) {
    	  		  for (j = 0; j < spp; j++) {
    		  		  real_arr[(imageLength * imageWidth * j) + imageLength * x + y] =
    			  	    (uint32_t) ((const uint32_t*)v)[j];
    			    }
      			} else if (bps == 32 && is_float) {
    	  		  for (j = 0; j < spp; j++) {
    		  		real_arr[(imageLength * imageWidth * j) + imageLength * x + y] =
    			  	  (double) ((const float*)v)[j];
    	  		  }
      			}
    			  x++;
    			  if (x >= imageWidth) {
      			  x -= imageWidth;
    	  		  y++;
    		  	}
    			}
    		} else {  // separate
  		  tsize_t step = bps / 8, i;
    		  for (i = 0; i < n; i += step) {
    			  const unsigned char *v = (const unsigned char*) buf + i;
    			  if (bps == 8) {
    			    real_arr[plane_offset + imageLength * x + y] = (uint8_t) v[0];
    			  }	else if (bps == 16) {
    			    real_arr[plane_offset + imageLength * x + y] =
    			      (uint16_t) ((const uint16_t*)v)[0];
    			  }	else if (bps == 32 && !is_float) {
    			    real_arr[plane_offset + imageLength * x + y] =
    			      (uint32_t) ((const uint32_t*)v)[0];
    			  }	else if (bps == 32 && is_float) {
    			    real_arr[plane_offset + imageLength * x + y] =
    			      (double) ((const float*)v)[0];
    		    }
    		    x++;
    		    if (x >= imageWidth) {
    			    x -= imageWidth;
    			    y++;
    			    if (y >= imageLength) {
    			      y -= imageLength;
    			      plane_offset += imageWidth * imageLength;
    			    }
    		    }
    		  }
    		}
    	}
  	} else {  // tiled image
  	  if (spp > 1 && config != PLANARCONFIG_CONTIG) {
  	    TIFFClose(tiff);
  	    Rf_error("Planar format tiled images are not supported");
  	  }

#if TIFF_DEBUG
  	  Rprintf(" - %d x %d tiles\n", TIFFNumberOfTiles(tiff), TIFFTileSize(tiff));
#endif
  	  x = 0; y = 0;
  	  buf = _TIFFmalloc(TIFFTileSize(tiff));

  	  for (y = 0; y < imageLength; y += tileLength) {
  	    for (x = 0; x < imageWidth; x += tileWidth) {
  	      tsize_t n = TIFFReadTile(tiff, buf, x, y, 0 /*depth*/, 0 /*plane*/);
  	      if (spp == 1) { // config doesn't matter for spp == 1
  	        // direct gray */
  	        tsize_t i, step = bps / 8;
  	        uint32 xoff = 0, yoff = 0;
  	        for (i = 0; i < n; i += step) {
  	          double val = NA_REAL;
  	          const unsigned char *v = (const unsigned char*) buf + i;
  	          if (bps == 8) {
  	            val = v[0];
  	          } else if (bps == 16) {
  	            val = ((const uint16_t*)v)[0];
  	          } else if (bps == 32) {
  	            if (is_float) {
  	              val = ((const float*)v)[0];
  	            } else {
  	              val = ((const uint32_t*)v)[0];
  	            }
  	          }
  	          if (x + xoff < imageWidth && y + yoff < imageLength)
  	            real_arr[imageLength * (x + xoff) + y + yoff] = val;
  	          xoff++;
  	          if (xoff >= tileWidth) {
  	            xoff -= tileWidth;
  	            yoff++;
  	          }
  	        }
  	      } else if (config == PLANARCONFIG_CONTIG) {  // spp > 1, interlaced
  	        tsize_t i, j, step = spp * bps / 8;
  	        uint32 xoff = 0, yoff = 0;
  	        for (i = 0; i < n; i += step) {
  	          const unsigned char *v = (const uint8_t*) buf + i;
  	          if (x + xoff < imageWidth && y + yoff < imageLength) {
  	            if (bps == 8) {
  	              for (j = 0; j < spp; j++)
  	                real_arr[(imageLength * imageWidth * j) +
  	                         imageLength * (x + xoff) + y + yoff] =
  	                  (double) v[j];
  	            } else if (bps == 16) {
  	              for (j = 0; j < spp; j++)
  	                real_arr[(imageLength * imageWidth * j) +
  	                         imageLength * (x + xoff) + y + yoff] =
  	                  (double) ((const uint16_t*)v)[j];
  	            } else if (bps == 32 && (!is_float)) {
  	              for (j = 0; j < spp; j++)
  	                real_arr[(imageLength * imageWidth * j) +
  	                         imageLength * (x + xoff) + y + yoff] =
  	                  (double) ((const uint32_t*)v)[j];
  	            } else if (bps == 32 && is_float) {
  	              for (j = 0; j < spp; j++)
  	                real_arr[(imageLength * imageWidth * j) +
  	                          imageLength * (x + xoff) + y + yoff] =
  	                  (double) ((const float*)v)[j];
  	            }
  	          }
  	          xoff++;
  	          if (xoff >= tileWidth) {
  	            xoff -= tileWidth;
  	            yoff++;
  	          }
  	        }
  	      }
  	    }
  	  }
  	}
  	_TIFFfree(buf);
  	dim = PROTECT(allocVector(INTSXP, (out_spp > 1) ? 3 : 2));
  	to_unprotect++;
  	INTEGER(dim)[0] = imageLength;
  	INTEGER(dim)[1] = imageWidth;
  	if (out_spp > 1) INTEGER(dim)[2] = out_spp;
  	setAttrib(res, R_DimSymbol, dim);
    TIFF_add_tags(tiff, res);
  	UNPROTECT(1);  // UNPROTECT `dim`
  	to_unprotect--;
  	n_img++;
  	if (multi_res == R_NilValue) {  // first image in stack
  	  multi_res = multi_tail = PROTECT(Rf_list1(res));
  	  to_unprotect++;  // `multi_res` needs to be UNPROTECTed later
  	} else {
  	  SEXP q = PROTECT(Rf_list1(res));
  	  to_unprotect++;
  	  SETCDR(multi_tail, q);  // `q` is now PROTECTed as part of `multi_tail`
  	  multi_tail = q;
  	  UNPROTECT(2);  // removing explit PROTECTion of `q` UNPROTECTing `res`
  	  to_unprotect -= 2;
  	}
  	if (!TIFFReadDirectory(tiff)) break;
  }
  TIFFClose(tiff);
  res = PROTECT(PairToVectorList(multi_res));  // convert LISTSXP into VECSXP
  to_unprotect++;
  UNPROTECT(to_unprotect);
  return res;
}

SEXP read_tags_C(SEXP sFn /*FileName*/, SEXP sAll) {
  check_type_sizes();
  uint64_t to_unprotect = 0;
  SEXP pre_res = PROTECT(R_NilValue), multi_res = PROTECT(R_NilValue);
  to_unprotect += 2;
  SEXP multi_tail = multi_res;
  const char *fn;
  int n_img = 0;
  tiff_job_t rj;
  TIFF *tiff;
  FILE *f;
  if (TYPEOF(sFn) != STRSXP || LENGTH(sFn) < 1) Rf_error("invalid filename");
  fn = CHAR(STRING_ELT(sFn, 0));
  f = fopen(fn, "rb");
  if (!f) Rf_error("unable to open %s", fn);
  rj.f = f;
  tiff = TIFF_Open("rmc", &rj); /* no mmap, no chopping */
  if (!tiff)
    Rf_error("Unable to open TIFF");
  uint64_t cur_dir = 0; /* 1-based image number */
  while (1) { /* loop over separate image in a directory if desired */
    cur_dir++;
    if (!isLogical(sAll)) {
      SEXP m = PROTECT(Rf_match(sAll, ScalarInteger(cur_dir), 0));
      int cur_dir_in_m = asInteger(m) == 0;
      UNPROTECT(1);  // UNPROTECT m as it's no longer needed
      if (cur_dir_in_m) {  /* No match */
        if (TIFFReadDirectory(tiff)) {
          continue;
        } else {
          break;
        }
      }
    }
    PROTECT(pre_res = allocVector(VECSXP, 0));
    to_unprotect++;
    TIFF_add_tags(tiff, pre_res);
    /* Build a linked list of results */
    n_img++;
    if (multi_res == R_NilValue) {  // first image in stack
      multi_res = multi_tail = PROTECT(Rf_list1(pre_res));
      to_unprotect++;  // `multi_res` needs to be UNPROTECTed later
    } else {
      SEXP q = PROTECT(Rf_list1(pre_res));
      to_unprotect++;
      SETCDR(multi_tail, q);  // `q` is now PROTECTed as part of `multi_tail`
      multi_tail = q;
      UNPROTECT(2);  // removing explit PROTECTion of `q` UNPROTECTing `pre_res`
      to_unprotect -= 2;
    }
    if (!TIFFReadDirectory(tiff))
      break;
  }
  TIFFClose(tiff);
  SEXP res = PROTECT(allocVector(VECSXP, n_img));
  to_unprotect++;  // PROTECT `res`
  SEXP multi_next = multi_res;
  for (R_len_t i = 0; i != n_img; ++i) {
    SEXP current_atts = PROTECT(ATTRIB(CAR(multi_next)));
    to_unprotect++;
    SET_VECTOR_ELT(res, i, Rf_PairToVectorList(current_atts));
    UNPROTECT(1);  // UNPROTECT `current_atts`
    to_unprotect--;
    multi_next = CDR(multi_next);
  }
  UNPROTECT(to_unprotect);
  return res;
}

SEXP count_directories_C(SEXP sFn /*FileName*/) {
  check_type_sizes();
  uint64_t to_unprotect = 0;
  SEXP res = PROTECT(allocVector(REALSXP, 1));
  to_unprotect++;
  const char *fn;
  tiff_job_t rj;
  TIFF *tiff;
  FILE *f;
  if (TYPEOF(sFn) != STRSXP || LENGTH(sFn) < 1) Rf_error("invalid filename");
  fn = CHAR(STRING_ELT(sFn, 0));
  f = fopen(fn, "rb");
  if (!f) Rf_error("unable to open %s", fn);
  rj.f = f;
  tiff = TIFF_Open("rmc", &rj); /* no mmap, no chopping */
  if (!tiff)
    Rf_error("Unable to open TIFF");
  uint64_t cur_dir = 0; /* 1-based image number */
  while (1) { /* loop over separate image in a directory if desired */
    cur_dir++;
    if (!TIFFReadDirectory(tiff)) break;
  }
  TIFFClose(tiff);
  REAL(res)[0] = cur_dir;
  UNPROTECT(to_unprotect);
  return res;
}
