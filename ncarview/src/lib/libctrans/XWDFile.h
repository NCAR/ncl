#include <X11/copyright.h>

/* Copyright 1985, 1986, Massachusetts Institute of Technology */

/* $XConsortium: XWDFile.h,v 1.8 88/09/06 15:55:58 jim Exp $ */
/*
 * XWDFile.h	MIT Project Athena, X Window system window raster
 *		image dumper, dump file format header file.
 *
 *  Author:	Tony Della Fera, DEC
 *		27-Jun-85
 * 
 * Modifier:    William F. Wyatt, SAO
 *              18-Nov-86  - version 6 for saving/restoring color maps
 */


/*
 * This is not portable between machines of differing word sizes.  To make
 * it portable, do the following things:
 *
 *     o  #include <X11/Xmd.h>
 *     o  remove the typedef for xwdval
 *     o  replace all instances of xwdval with the appropriate CARD32 ... B32
 *     o  make sure that XWDFileHeader is padded to quadword boundaries
 *     o  make sure the window name is written out quadword aligned
 *     o  create an XWDColor structure that contains the same fields as XColor
 *        but which is defined in terms of CARD32 B32, CARD16 B16, and CARD8
 *     o  convert XColor structures to XWDColor structures in xwd
 *     o  remove all xwdval casts from xwd
 *     o  pack image data before writing out if necessary
 *     o  replace casts from xwdval objects in xwud with cvtINT macros
 *     o  convert XWDColor structures to XColor structures
 *     o  unpack data after reading in if necessary
 */


typedef unsigned long xwdval;

#define XWD_FILE_VERSION 7

typedef struct _xwd_file_header {
	xwdval header_size;	  /* Size of the entire file header (bytes). */
	xwdval file_version;	  /* XWD_FILE_VERSION */
	xwdval pixmap_format;	  /* Pixmap format */
	xwdval pixmap_depth;	  /* Pixmap depth */
	xwdval pixmap_width;	  /* Pixmap width */
	xwdval pixmap_height;	  /* Pixmap height */
	xwdval xoffset;           /* Bitmap x offset */
	xwdval byte_order;        /* MSBFirst, LSBFirst */
	xwdval bitmap_unit;       /* Bitmap unit */
	xwdval bitmap_bit_order;  /* MSBFirst, LSBFirst */
	xwdval bitmap_pad;	  /* Bitmap scanline pad */
	xwdval bits_per_pixel;	  /* Bits per pixel */
	xwdval bytes_per_line;	  /* Bytes per scanline */
	xwdval visual_class;	  /* Class of colormap */
	xwdval red_mask;	  /* Z red mask */
	xwdval green_mask;	  /* Z green mask */
	xwdval blue_mask;	  /* Z blue mask */
	xwdval bits_per_rgb;	  /* Log base 2 of distinct color values */
	xwdval colormap_entries;  /* Number of entries in colormap */
	xwdval ncolors;		  /* Number of Color structures */
	xwdval window_width;	  /* Window width */
	xwdval window_height;	  /* Window height */
	long window_x;		  /* Window upper left X coordinate */
	long window_y;		  /* Window upper left Y coordinate */
	xwdval window_bdrwidth;	  /* Window border width */
} XWDFileHeader;
