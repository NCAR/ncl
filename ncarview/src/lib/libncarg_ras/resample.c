/*
 *	$Id: resample.c,v 1.14 2008-07-27 03:18:46 haley Exp $
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

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	resample.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	History:
 *		Originally written 11/91		Don Middleton
 *		Added nearest neighbor support 3/92	Don Middleton
 *
 *	Description:
 *		This file contains image resampling functions.
 *		
 */
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<math.h>
#include	"ncarg_ras.h"

/*LINTLIBRARY*/

/**********************************************************************
 *	Function: RasResampleBilinear(src, dst, verbose)
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/92
 *
 *	Description:
 *		"src" is a pointer to a source raster structure
 *		of any encoding type. "dst" must be a pointer
 *		to a preallocated raster structure of type 
 *		RAS_DIRECT. The size of "dst" is the size of
 *		the resampled image. Aspect ratio is not necessarily
 *		preserved. If "verbose" is not equal to zero, the 
 *		function will report it's progress on stderr.
 *
 *	Comments:
 *		Gives nice results but is damn slow.
 *		
 *********************************************************************/
int	RasterResampleBilinear(src, dst, verbose)
	Raster	*src;
	Raster	*dst;
	int	verbose;
{
	Raster		*RasterCreate();
	float		fx, fy;
	float		i1, i2; /* Intermediate color intensity */
	int		*x1v;
	int		x1, x2;
	int		y1, y2;
	int		dx, dy;
	float		*fracxv;
	float		fracx, fracy;
	float		p11, p12, p21, p22;
	unsigned char	*rgbptr11, *rgbptr12, *rgbptr21, *rgbptr22;

	/*
	Output object must be RAS_DIRECT because interpolation is 
	in RGB color space.
	*/

	if (dst->type != RAS_DIRECT) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"Resampling can only be done to direct color raster");
		return(RAS_ERROR);
	}

	/*
	"dx" and "dy" are the discrete coordinates of the output image.
	They are inverse-mapped into the continuous coordinates of
	the input image and bilinear interpolation is used to
	determine the new image values at the new discrete coordinates.
	*/

	/*
	"fx" and "fy" are coordinates in the continuous input
	image space. The vector "fx" is precomputed for efficiency.
	*/
	x1v = (int *) ras_malloc(dst->nx*sizeof(int));
	fracxv = (float *) ras_malloc(dst->nx*sizeof(float));

	for(dx=0; dx<dst->nx; dx++) {
		fx = (float)dx*(((float)src->nx - 1)/((float)dst->nx - 1));
		x1v[dx] = floor(fx);
		fracxv[dx] = fx - x1v[dx];
	}

	for(dy=0; dy<dst->ny; dy++) {
		fy = dy * (((float)src->ny - 1)/((float)dst->ny - 1));
		y1 = floor(fy);
		y2 = y1 < src->ny-1 ? y1 + 1 : y1;
		fracy  = fy - y1;
		if (verbose && dy % 50 == 0) {
			(void) fprintf(stderr, "Reconstructing row %d\n", dy);
		}
		for(dx=0; dx<dst->nx; dx++) {
			x1 = x1v[dx];
			x2 = x1 < src->nx-1 ? x1 + 1 : x1;
			fracx  = fracxv[dx];

			rgbptr11 = &src->data[y1*3*src->nx + x1*3];
			rgbptr21 = &src->data[y1*3*src->nx + x2*3];
			rgbptr12 = &src->data[y2*3*src->nx + x1*3];
			rgbptr22 = &src->data[y2*3*src->nx + x2*3];

			if (src->type == RAS_DIRECT) {
				p11 = (float) *rgbptr11;
				p12 = (float) *rgbptr12;
				p21 = (float) *rgbptr21;
				p22 = (float) *rgbptr22;
			}
			else if (src->type == RAS_INDEXED) {
				p11 = src->red[INDEXED_PIXEL(src, x1, y1)];
				p12 = src->red[INDEXED_PIXEL(src, x1, y2)];
				p21 = src->red[INDEXED_PIXEL(src, x2, y1)];
				p22 = src->red[INDEXED_PIXEL(src, x2, y2)];
			}
			i1 = p11 + fracy * (p12 - p11);
			i2 = p21 + fracy * (p22 - p21);

			DIRECT_RED(dst, dx, dy) = (int)(i1 + fracx * (i2 - i1));

			if (src->type == RAS_DIRECT) {
				p11 = (float) *(rgbptr11+1);
				p12 = (float) *(rgbptr12+1);
				p21 = (float) *(rgbptr21+1);
				p22 = (float) *(rgbptr22+1);
			}
			else if (src->type == RAS_INDEXED) {
				p11 = src->green[INDEXED_PIXEL(src, x1, y1)];
				p12 = src->green[INDEXED_PIXEL(src, x1, y2)];
				p21 = src->green[INDEXED_PIXEL(src, x2, y1)];
				p22 = src->green[INDEXED_PIXEL(src, x2, y2)];
			}

			i1 = p11 + fracy * (p12 - p11);
			i2 = p21 + fracy * (p22 - p21);

			DIRECT_GREEN(dst, dx, dy) = 
				(int) (i1 + fracx * (i2 - i1));

			if (src->type == RAS_DIRECT) {
				p11 = (float) *(rgbptr11+2);
				p12 = (float) *(rgbptr12+2);
				p21 = (float) *(rgbptr21+2);
				p22 = (float) *(rgbptr22+2);
			}
			else if (src->type == RAS_INDEXED) {
				p11 = src->blue[INDEXED_PIXEL(src, x1, y1)];
				p12 = src->blue[INDEXED_PIXEL(src, x1, y2)];
				p21 = src->blue[INDEXED_PIXEL(src, x2, y1)];
				p22 = src->blue[INDEXED_PIXEL(src, x2, y2)];
			}

			i1 = p11 + fracy * (p12 - p11);
			i2 = p21 + fracy * (p22 - p21);

			DIRECT_BLUE(dst, dx, dy) = 
				(int) (i1 + fracx * (i2 - i1));
	}}

	(void) ras_free((char *) x1v);
	(void) ras_free((char *) fracxv);

	return(RAS_OK);
}

/**********************************************************************
 *	Function: RasResampleNearestNeighbor(src, dst, verbose)
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/92
 *
 *	Description:
 *		"src" is a pointer to a source raster structure
 *		of any encoding type. "dst" is a pointer
 *		to the destination raster and can be of
 *		the same type as the source, or RGB_DIRECT.
 *		The size of "dst" is the size of the resampled 
 *		image. Aspect ratio is not necessarily preserved.
 *		If "verbose" is not equal to zero, the function 
 *		will report it's progress on stderr.
 *
 *	Comments:
 *		Each pixel in the output image is reverse mapped
 *		into the continuous coordinate space of the
 *		input image. The new pixel is simply assigned
 *		the color of the closest input pixel. While this
 *		is much faster than bilinear interpolation, it
 *		can produce poor results. The advantage of
 *		this approach over pixel replication/averaging
 *		is that the resolution of the output image is 
 *		not restricted to being an integer multiple or
 *		divisor of the input image.
 *		
 *********************************************************************/
int	RasterResampleNearestNeighbor(src, dst, verbose)
	Raster	*src;
	Raster	*dst;
	int	verbose;
{
	Raster		*RasterCreate();
	float		fx, fy;
	int		dx, dy;
	int		*sx, sy;
	unsigned char	*srcptr, *dstptr;

	if (src->type != dst->type) {
		(void) ESprintf(RAS_E_INTERNAL,
		"RasterResampleNearestNeighbor() - src and dst not same type");
		return(RAS_ERROR);
	}

	/*
	In all cases, the output image will occupy precisely the same 
	color space as the input image.
	*/

	if (src->type == RAS_INDEXED) {
		(void) RasterCopyColormap(src, dst);
	}

	/*
	"dx" and "dy" are the discrete coordinates of the output image.
	They are inverse-mapped into the continuous coordinates of
	the input image and the new output pixel is simply the
	same as the closest one in the input image. The variables
	"fx" and "fy" are coordinates in the continuous input
	image space. The integer vector "sx" is precomputed
	for efficiency.
	*/

	sx = (int *) ras_malloc(dst->nx*sizeof(int));

	for(dx=0; dx<dst->nx; dx++) {
		fx = (float)dx*(((float)src->nx - 1)/((float)dst->nx - 1));
		sx[dx] = (int) (fx + .5);
		if (sx[dx] > (src->nx-1) ) sx[dx] = src->nx - 1;
	}

	for(dy=0; dy<dst->ny; dy++) {
		fy = dy * (((float)src->ny - 1)/((float)dst->ny - 1));
		sy = (int) (fy + .5);
		if (sy > (src->ny-1) ) sy = src->ny - 1;

		if (verbose && dy % 50 == 0) {
			(void) fprintf(stderr, "Reconstructing row %d\n", dy);
		}

		if (src->type == RAS_DIRECT) {
			dstptr = &DIRECT_RED(dst, 0, dy);
			for(dx=0; dx<dst->nx; dx++) {
				srcptr = &DIRECT_RED(src, sx[dx], sy);
				*dstptr++ = *srcptr++;
				*dstptr++ = *srcptr++;
				*dstptr++ = *srcptr++;
			}
		}
		else if (src->type == RAS_INDEXED) {
		  dstptr = &INDEXED_PIXEL(dst, 0, dy);
		  for(dx=0; dx<dst->nx; dx++) {
		    *dstptr++ = INDEXED_PIXEL(src, sx[dx], sy);
		  }
		}
	}

	(void) ras_free((char *) sx);

	return(RAS_OK);
}

/**********************************************************************
 *	Function: RasterScale(src, scale)
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/91
 *
 *	Description:
 *		This routine is due to be replaced with a more
 *		general integer pixel replication/averaging
 *		routine. Don't write to it.
 *		
 *********************************************************************/
Raster *
RasterScale(src, scale)
	Raster	*src;
	int	scale;
{
	Raster		*RasterCreate();
	Raster		*new = (Raster *) NULL;
	Raster		*result;
	int		sx, sy, dx, dy;
	int		pixel;
	unsigned char	ip00, ip10, ip01, ip11;
	unsigned char	red, green, blue;


	if (scale == -2) {
		scale = abs(scale);
		new = RasterCreate(src->nx/scale, src->ny/scale, RAS_DIRECT);

		for(dy = 0; dy < new->ny; dy++) {
		for(dx = 0; dx < new->nx; dx++) {
		  sx = dx * 2; sy = dy * 2;

		  if (src->type == RAS_INDEXED) {
		    ip00 = INDEXED_PIXEL(src,sx,sy);
		    ip10 = INDEXED_PIXEL(src,sx+1,sy);
		    ip01 = INDEXED_PIXEL(src,sx,sy+1);
		    ip11 = INDEXED_PIXEL(src,sx+1,sy+1);

		    red = (int) (INDEXED_RED(src, ip00)+INDEXED_RED(src, ip10) +
			   INDEXED_RED(src, ip01)+INDEXED_RED(src, ip11) ) / 4;

		    green = (int) (INDEXED_GREEN(src, ip00) +
				INDEXED_GREEN(src, ip10)+
				INDEXED_GREEN(src, ip01)+
				INDEXED_GREEN(src, ip11)) / 4;

		    blue = (int) (INDEXED_BLUE(src, ip00) +
				INDEXED_BLUE(src, ip10) +
				INDEXED_BLUE(src, ip01) +
				INDEXED_BLUE(src, ip11)) / 4;
		  }
		  else if (src->type == RAS_DIRECT) {
		    red = (int) (DIRECT_RED(src,sx,sy) +
			   DIRECT_RED(src,sx+1,sy) +
			   DIRECT_RED(src,sx,sy+1) +
			   DIRECT_RED(src,sx+1,sy+1)) / 4;
		    green = (int) (DIRECT_GREEN(src,sx,sy) +
			   DIRECT_GREEN(src,sx+1,sy) +
			   DIRECT_GREEN(src,sx,sy+1) +
			   DIRECT_GREEN(src,sx+1,sy+1)) / 4;
		    blue = (int) (DIRECT_BLUE(src,sx,sy) +
			   DIRECT_BLUE(src,sx+1,sy) +
			   DIRECT_BLUE(src,sx,sy+1) +
			   DIRECT_BLUE(src,sx+1,sy+1)) / 4;
		  }

		  DIRECT_RED(new, dx, dy) = red;
		  DIRECT_GREEN(new, dx, dy) = green;
		  DIRECT_BLUE(new, dx, dy) = blue;
		}}
		result = new;
	}
	else if (scale < 1) {
		(void) fprintf(stderr, "Negative scaling not supported\n");
		scale = 1;
	}
	else if (scale > 1) {
		new = RasterCreate(src->nx/scale, src->ny/scale, RAS_DIRECT);

		if (src->type == RAS_INDEXED) {
			COPY_COLORMAP(src, new);

			for(sy=0; sy<src->ny; sy++)
			for(sx=0; sx<src->nx; sx++) {
				pixel = INDEXED_PIXEL(src, sx, sy);

				for(dy=sy*scale; dy<sy*scale+scale; dy++)
				for(dx=sx*scale; dx<sx*scale+scale; dx++)
				  INDEXED_PIXEL(new,dx,dy) = pixel;
			}
		}
		else if (src->type == RAS_DIRECT) {
			(void) fprintf(stderr, 
				"Scaling not supported for True Color\n");

		}
		result = new;
	}
	else {
		result = src;
	}

	return(result);
}

/**********************************************************************
 *	Function: RasterAverage(src, scale)
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/91
 *
 *	Description:
 *		This routine is due to be replaced with a more
 *		general integer pixel replication/averaging
 *		routine. Don't write to it.
 *		
 *********************************************************************/
int
RasterAverage(src, dst)
	Raster	*src, *dst;
{
	int		sx, sy, dx, dy;
	unsigned char	ip00, ip10, ip01, ip11;
	unsigned char	red, green, blue;

	for(dy = 0; dy < dst->ny; dy++) {
	for(dx = 0; dx < dst->nx; dx++) {
		sx = dx * 2; sy = dy * 2;

		if (src->type == RAS_INDEXED) {
		  ip00 = INDEXED_PIXEL(src,sx,sy);
		  ip10 = INDEXED_PIXEL(src,sx+1,sy);
		  ip01 = INDEXED_PIXEL(src,sx,sy+1);
		  ip11 = INDEXED_PIXEL(src,sx+1,sy+1);

		  red = (int) (INDEXED_RED(src, ip00)+INDEXED_RED(src, ip10) +
			 INDEXED_RED(src, ip01)+INDEXED_RED(src, ip11) ) / 4;

		  green = (int) (INDEXED_GREEN(src, ip00) +
			INDEXED_GREEN(src, ip10)+
			INDEXED_GREEN(src, ip01)+
			INDEXED_GREEN(src, ip11)) / 4;

		  blue = (int) (INDEXED_BLUE(src, ip00) +
			INDEXED_BLUE(src, ip10) +
			INDEXED_BLUE(src, ip01) +
			INDEXED_BLUE(src, ip11)) / 4;
		}
		else if (src->type == RAS_DIRECT) {
		  red = (int) (DIRECT_RED(src,sx,sy) +
			 DIRECT_RED(src,sx+1,sy) +
			 DIRECT_RED(src,sx,sy+1) +
			 DIRECT_RED(src,sx+1,sy+1)) / 4;
		  green = (int) (DIRECT_GREEN(src,sx,sy) +
			 DIRECT_GREEN(src,sx+1,sy) +
			 DIRECT_GREEN(src,sx,sy+1) +
			 DIRECT_GREEN(src,sx+1,sy+1)) / 4;
		  blue = (int) (DIRECT_BLUE(src,sx,sy) +
			 DIRECT_BLUE(src,sx+1,sy) +
			 DIRECT_BLUE(src,sx,sy+1) +
			 DIRECT_BLUE(src,sx+1,sy+1)) / 4;
		}

		DIRECT_RED(dst, dx, dy) = red;
		DIRECT_GREEN(dst, dx, dy) = green;
		DIRECT_BLUE(dst, dx, dy) = blue;
	}}

	return(RAS_OK);
}
