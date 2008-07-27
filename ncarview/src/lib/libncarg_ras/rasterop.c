/*
 * $Id: rasterop.c,v 1.6 2008-07-27 03:18:46 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <errno.h>
#include "ncarg_ras.h"

static int	OptionCenter = True;

void	RasterCopy(src, dst, src_x, src_y, src_nx, src_ny)
	Raster	*src;
	Raster	*dst;
	int	src_x, src_y, src_nx, src_ny;
{
	int	x, y;
	int	r, g, b;
	int	x1, y1, x2, y2;

	x1 = src_x;
	y1 = src_y;
	x2 = src_x + src_nx - 1;
	y2 = src_y + src_ny - 1;

	if (src->type == RAS_INDEXED)
		RasterCopyColormap(src, dst);
	
	for(y=y1; y<=y2; y++)
	for(x=x1; x<=x2; x++)
	{
		if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);
			DIRECT_RED(dst, x, y) = r;
			DIRECT_GREEN(dst, x, y) = g;
			DIRECT_BLUE(dst, x, y) = b;
		}
		else if (src->type == RAS_INDEXED) {
			INDEXED_PIXEL(dst, x, y) = INDEXED_PIXEL(src, x, y);
		}
	}
}

/* ARGSUSED */
void	RasterOp(src, dst, src_x, src_y, src_nx, src_ny, dst_x, dst_y, op)
	Raster	*src;
	Raster	*dst;
	int	src_x, src_y, src_nx, src_ny, dst_x, dst_y;
	int	op;
{
	int	r, g, b;
	int	sx, sy, dx, dy;
	int	sx1, sx2, sy1, sy2;

	sx1 = src_x;
	sx2 = src_x + src_nx - 1;
	sy1 = src_y;
	sy2 = src_y + src_ny - 1;
	
	for(sy=sy1, dy=dst_y; sy<=sy2; sy++, dy++)
	for(sx=sx1, dx=dst_x; sx<=sx2; sx++, dx++)
	{
		if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, sx, sy);
			g = DIRECT_GREEN(src, sx, sy);
			b = DIRECT_BLUE(src, sx, sy);
			DIRECT_RED(dst, dx, dy) = r;
			DIRECT_GREEN(dst, dx, dy) = g;
			DIRECT_BLUE(dst, dx, dy) = b;
		}
		else if (src->type == RAS_INDEXED) {
			INDEXED_PIXEL(dst, dx, dy) = INDEXED_PIXEL(src, sx, sy);
		}
	}
}

int
RasterCenterCrop(src, dst)
	Raster		*src;
	Raster		*dst;
{
	char			*errmsg = "RasterCenterCrop(\"%s\",\"%s\")";
	int			sx, sy, dy;	/* source and dest indices */
	int			src_x, src_y;	/* source upper-left corner */
	int			src_nx, src_ny;	/* source extent */
	int			dst_x, dst_y;	/* dest upper-left corner */
	unsigned char		pixel, *src_ptr, *dst_ptr;

	/* Check for incompatible encodings. */

	if (src->type == RAS_DIRECT && dst->type == RAS_INDEXED) {
		(void) ESprintf(RAS_E_PROGRAMMING,errmsg,src->name,dst->name);
		return(RAS_ERROR);
	}

	/* Set defaults for image positioning. */

	src_x = 0; src_y = 0;
	src_nx = src->nx; src_ny = src->ny;
	dst_x = 0; dst_y = 0;

	/* Calculate X mapping */

	if (src->nx > dst->nx) {
		dst_x = 0;
		src_nx = dst->nx;
		if (OptionCenter)
			src_x = (src->nx - dst->nx) / 2;
		else
			src_x = 0;
	}
	else {
		src_x = 0;
		src_nx = src->nx;
		if (OptionCenter)
			dst_x = (dst->nx - src_nx) / 2;
		else
			dst_x = 0;
	}

	/* Calculate Y mapping */

	if (src->ny >= dst->ny) {
		dst_y = 0;
		src_ny = dst->ny;
		if (OptionCenter)
			src_y = (src->ny - dst->ny) / 2;
		else
			src_y = 0;
	}
	else {
		src_y = 0;
		src_ny = src->ny;
		if (OptionCenter)
			dst_y = (dst->ny - src_ny) / 2;
		else
			dst_y = 0;
	}

	/* Copy colormaps for all-indexed encodings. */

	if (src->type == RAS_INDEXED && dst->type == RAS_INDEXED) {
		(void) RasterCopyColormap(src, dst);
	}

	/*
	In order to avoid repetitive and costly address arithmetic,
	this loop works with pointers more than would be ideal.
	*/

	for(sy=src_y, dy=dst_y; sy<src_y+src_ny; sy++, dy++) {

		if (src->type == RAS_INDEXED) {
			src_ptr = &INDEXED_PIXEL(src, src_x, sy);
		}
		else if (src->type == RAS_DIRECT) {
			src_ptr = &DIRECT_RED(src, src_x, sy);
		}

		if (dst->type == RAS_INDEXED) {
			dst_ptr = &INDEXED_PIXEL(dst, dst_x, dy);
		}
		else if (dst->type == RAS_DIRECT) {
			dst_ptr = &DIRECT_RED(dst, dst_x, dy);
		}

		for(sx=src_x; sx<src_x+src_nx; sx++) {
			if (src->type  == RAS_INDEXED) {
				pixel = *src_ptr++;
				if (dst->type == RAS_INDEXED) {
					*dst_ptr++ = pixel;
				}
				else if (dst->type == RAS_DIRECT) {
					*dst_ptr++ = src->red[pixel];
					*dst_ptr++ = src->green[pixel];
					*dst_ptr++ = src->blue[pixel];
				}
			}
			else if (src->type == RAS_DIRECT) {
				if (dst->type == RAS_DIRECT) {
					*dst_ptr++ = *src_ptr++;
					*dst_ptr++ = *src_ptr++;
					*dst_ptr++ = *src_ptr++;
				}
			}
		}
	}

	return(RAS_OK);
}

/*
 * Function:		RasterInvert(src, dst)
 *
 * Description:		Performs an source-to-destination or
 *			in-place vertical image inversion. If
 *			"src" and "dst" are the same, then
 *			an in-place conversion is done. Otherwise,
 *			The "src" raster is copied to the "dst"
 *			raster with inversion taking place in
 *			the process. If "src" and "dst" are not
 *			the same size or encoding, "src" is
 *			cropped, centered, and re-encoded as
 *			appropriate.
 *
 * In Args:		Raster	*src;
 *			Raster	*dst;
 *
 * Out Args:		Raster	*src;
 *			Raster	*dst;
 *
 * Return Values:	RAS_OK or RAS_ERROR
 *
 * SideEffect:		If "src" and "dst" are the same, "src->data"
 *			is modified. Otherwise, "dst->data" is
 *			modified.
 */
int
RasterInvert(src, dst)
	Raster		*src;
	Raster		*dst;
{
	int		status;
	static char	*errmsg = "RasterInvert(\"%s\")";
	unsigned char	*linebuf, *top_ptr, *bot_ptr;
	unsigned int	length;
	unsigned int	y;
	Raster		*ras;

	/* If "src" and "dst" are different, copy over the image. */

	if (src != dst) {
		status = RasterCenterCrop(src, dst);
		if (status != RAS_OK) return(status);
	}

	/* Now invert "dst". */

	ras = dst;

	/* Calculate length of row. */
	if (ras->type == RAS_INDEXED) {
		length = ras->nx;
	}
	else if (ras->type == RAS_DIRECT) {
		length = ras->nx * 3;
	}

	/* Allocate a temporary line buffer. */

	linebuf = (unsigned char *) calloc(length, 1);
	if (linebuf == (unsigned char *) NULL) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_ERROR);
	}

	for(y=0; y<ras->ny/2; y++) {
		if (ras->type == RAS_INDEXED) {
			top_ptr = &INDEXED_PIXEL(ras, 0, y);
			bot_ptr = &INDEXED_PIXEL(ras, 0, ras->ny-y-1);
		}
		else if (ras->type == RAS_DIRECT) {
			top_ptr = &DIRECT_RED(ras, 0, y);
			bot_ptr = &DIRECT_RED(ras, 0, ras->ny-y-1);
		}

		(void) memcpy(linebuf, top_ptr, length);
		(void) memcpy(top_ptr, bot_ptr, length);
		(void) memcpy(bot_ptr, linebuf, length);
	}

	(void) free(linebuf);
	return(RAS_OK);
}
