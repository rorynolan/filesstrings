#include "common.h"
#include <stdlib.h>
#include <string.h>

#include <Rinternals.h>

static int need_init = 1;

static char txtbuf[2048];  // text buffer

static TIFF *last_tiff; /* this to avoid leaks */

static void TIFFWarningHandler_(const char* module, const char* fmt, va_list ap) {
  /* we can't pass it directly since R has no vprintf entry point */
  // FIXME: could possible put an if statement here to suppress tag warnings
  vsnprintf(txtbuf, sizeof(txtbuf), fmt, ap);
  if (strstr(txtbuf,
             "Unknown field with tag 50838 (0xc696) encountered") == NULL &&
        strstr(txtbuf,
               "Unknown field with tag 50839 (0xc697) encountered") == NULL &&
        strstr(txtbuf,
               "Defining non-color channels as ExtraSamples.") == NULL) {
  Rf_warning("%s: %s", module, txtbuf);
  }
}

static int err_reenter = 0;

static void TIFFErrorHandler_(const char* module, const char* fmt, va_list ap) {
  if (err_reenter) return;
  /* prevent re-entrance which can happen as TIFF
     is happy to call another error from Close */
  err_reenter = 1;
  /* we can't pass it directly since R has no vprintf entry point */
  vsnprintf(txtbuf, sizeof(txtbuf), fmt, ap);
  /* we have to close the TIFF that caused it as it will not
     come back -- recursive calls won't work under errors
     but that is hopefully unlikely/impossible */
  Rf_warning("The tiff file you are attempting to read from is causing the "
               "following problem: \"%s: %s\"", module, txtbuf);
  if (last_tiff) TIFFClose(last_tiff); /* this will also reset last_tiff */
  err_reenter = 0;
  Rf_error("%s: %s", module, txtbuf);
}

static void init_tiff() {
  if (need_init) {
  	TIFFSetWarningHandler(TIFFWarningHandler_);
	  TIFFSetErrorHandler(TIFFErrorHandler_);
  	need_init = 0;
  }
}

static tsize_t TIFFReadProc_(thandle_t usr, tdata_t buf, tsize_t length) {
  tiff_job_t *rj = (tiff_job_t*) usr;  // rj is read_job
  tsize_t to_read = length;
  if (rj->f) return fread(buf, 1, to_read, rj->f);
  #if TIFF_DEBUG
    Rprintf("read [@%d %d/%d] -> %d\n", rj->ptr, rj->len, rj->alloc, length);
  #endif
  if (to_read > (rj->len - rj->ptr)) to_read = (rj->len - rj->ptr);
  if (to_read > 0) {
  	memcpy(buf, rj->data + rj->ptr, to_read);
	  rj->ptr += to_read;
  }
  return to_read;
}

static int guarantee_write_buffer(tiff_job_t *rj, long where) {
  if (where > rj->alloc) { // if need to resize buffer
  	void *new_data;
	  unsigned long new_alloc = rj->alloc;
  	while (new_alloc <= where) new_alloc <<= 1;
	  new_data = realloc(rj->data, new_alloc);
	  if (!new_data) {// if FAILED to resize
	    return 0;
	  }
	  rj->data = new_data;
	  rj->alloc = new_alloc;
  }
  return 1;
}

static tsize_t TIFFWriteProc_(thandle_t usr, tdata_t buf, tsize_t length) {
  tiff_job_t *rj = (tiff_job_t*) usr;
  if (rj->f) return (tsize_t) fwrite(buf, 1, length, rj->f);
  #if TIFF_DEBUG
    Rprintf("write [@%d %d/%d] <- %d\n", rj->ptr, rj->len, rj->alloc, length);
  #endif
  if (!guarantee_write_buffer(rj, rj->ptr + length)) return 0;
  memcpy(rj->data + rj->ptr, buf, length);
  rj->ptr += length;
  if (rj->ptr > rj->len) rj->len = rj->ptr;
  return length;
}

static toff_t  TIFFSeekProc_(thandle_t usr, toff_t offset, int whence) {
  tiff_job_t *rj = (tiff_job_t*) usr;
  if (rj->f) {
  	int e = fseeko(rj->f, offset, whence);
	  if (e != 0) {
	    Rf_warning("fseek failed on a file in TIFFSeekProc");
	    return -1;
	  }
	return ftello(rj->f);
  }
  #if TIFF_DEBUG
    Rprintf("seek [@%d %d/%d]  %d (%d)\n", rj->ptr, rj->len, rj->alloc, offset, whence);
  #endif
  if (whence == SEEK_CUR)	{
    offset += rj->ptr;
  } else if (whence == SEEK_END) {
  	offset += rj->len;
  } else if (whence != SEEK_SET) {
  	Rf_warning("invalid `whence' argument to TIFFSeekProc callback called by libtiff");
	  return -1;
  }
  if (rj->alloc && rj->len < offset) {
	  if (offset >= rj->alloc) { // if need more space
	    if (!guarantee_write_buffer(rj, offset)) {
		    return -1;
	    }
	  } else {  // enough space but need to zero out
	    memset(rj->data + rj->len, 0, offset - rj->len);
	  }
	  rj->len = offset;
  }

  if (offset > rj->len) {
	  Rf_warning("libtiff attempted to seek beyond the data end");
	  return -1;
  }
  return (toff_t) (rj->ptr = offset);
}

static int TIFFCloseProc_(thandle_t usr) {
  tiff_job_t *rj = (tiff_job_t*) usr;
  if (rj->f) {
    fclose(rj->f);
  } else if (rj->alloc) {
  	free(rj->data);
	  rj->data = 0;
  	rj->alloc = 0;
  }
  last_tiff = 0;
  return 0;
}

static toff_t TIFFSizeProc_(thandle_t usr) {
  tiff_job_t *rj = (tiff_job_t*) usr;
  if (rj->f) {
	  off_t cur = ftello(rj->f), end;
  	fseek(rj->f, 0, SEEK_END);
  	end = ftello(rj->f);
	  fseeko(rj->f, cur, SEEK_SET);
	  return end;
  }
  return (toff_t) rj->len;
}

static int TIFFMapFileProc_(thandle_t usr, tdata_t* map, toff_t* off) {
  Rf_warning("libtiff attempted to use TIFFMapFileProc on non-file which is unsupported");
  return -1;
}

static void TIFFUnmapFileProc_(thandle_t usr, tdata_t map, toff_t off) {
  Rf_warning("libtiff attempted to use TIFFUnmapFileProc on non-file which is unsupported");
}

/* actual interface */
TIFF *TIFF_Open(const char *mode, tiff_job_t *rj) {
  if (need_init) init_tiff();
  #if AGGRESSIVE_CLEANUP
    if (last_tiff) TIFFClose(last_tiff);
  #endif
  return(last_tiff =
	         TIFFClientOpen("pkg:tiff", mode, (thandle_t) rj, TIFFReadProc_,
                          TIFFWriteProc_, TIFFSeekProc_, TIFFCloseProc_,
                          TIFFSizeProc_, TIFFMapFileProc_, TIFFUnmapFileProc_));
}

void check_type_sizes(void) {
  unsigned int sz = sizeof(unsigned char) * CHAR_BIT;
  if (sz != 8) {
    Rf_error("Usually, the size of the \'unsigned char\' type is 8 bits, "
               "however on your system it is %i. The \'ijtiff\' library relies "
               "on \'unsigned char\' being 8-bit, so it will not work "
               "on your system.", sz);
  }
  sz = sizeof(unsigned short) * CHAR_BIT;
  if (sz != 16) {
    Rf_error("Usually, the size of the \'unsigned short\' type is 16 bits, "
               "however on your system it is %i. The \'ijtiff\' library relies "
               "on \'unsigned short\' being 16-bit, so it will not "
               "work on your system.", sz);
  }
  sz = sizeof(unsigned int) * CHAR_BIT;
  if (sz != 32) {
    Rf_error("Usually, the size of the \'unsigned int\' type is 32 bits, "
               "however on your system it is %i. The \'ijtiff\' library relies "
               "on \'unsigned int\' being 32-bit, so it will not work "
               "on your system.", sz);
  }
  sz = sizeof(unsigned long long int) * CHAR_BIT;
  if (sz != 64) {
    Rf_error("Usually, the size of the \'unsigned long long int\' type is "
               "64 bits, however on your system it is %i. The \'ijtiff\' "
               "library relies on \'unsigned int\' being 32-bit, so it will not"
               " work on your system.", sz);
  }
  sz = sizeof(float) * CHAR_BIT;
  if (sz != 32) {
    Rf_error("Usually, the size of the \'float\' type is 32 bits, "
               "however on your system it is %i. The \'ijtiff\' library relies "
               "on \'float\' being 32-bit, so it will not work "
               "on your system.", sz);
  }
}
