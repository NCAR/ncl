/*
 *	$Id: resample.c,v 1.3 1992-02-21 12:49:35 clyne Exp $
 */
#include	<stdio.h>
#include	<string.h>
#include	<math.h>
#include	<malloc.h>
#include	"ncarg_ras.h"

Raster *
RasScale(src, scale)
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

		    red = (INDEXED_RED(src, ip00)+INDEXED_RED(src, ip10) +
			   INDEXED_RED(src, ip01)+INDEXED_RED(src, ip11) ) / 4;

		    green = (INDEXED_GREEN(src, ip00)+INDEXED_GREEN(src, ip10)+
			   INDEXED_GREEN(src, ip01)+INDEXED_GREEN(src, ip11))/4;

		    blue = (INDEXED_BLUE(src, ip00)+INDEXED_BLUE(src, ip10) +
			   INDEXED_BLUE(src, ip01)+INDEXED_BLUE(src, ip11))/4;
		  }
		  else if (src->type == RAS_DIRECT) {
		    red = (DIRECT_RED(src,sx,sy) +
			   DIRECT_RED(src,sx+1,sy) +
			   DIRECT_RED(src,sx,sy+1) +
			   DIRECT_RED(src,sx+1,sy+1)) / 4;
		    green = (DIRECT_GREEN(src,sx,sy) +
			   DIRECT_GREEN(src,sx+1,sy) +
			   DIRECT_GREEN(src,sx,sy+1) +
			   DIRECT_GREEN(src,sx+1,sy+1)) / 4;
		    blue = (DIRECT_BLUE(src,sx,sy) +
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

Raster *
RasScaleBilinear(src, nx, ny, verbose)
	Raster	*src;
	int	nx, ny;
	int	verbose;
{
	Raster		*RasterCreate();
	Raster		*dst = (Raster *) NULL;
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

	if (nx == src->nx || ny == src->ny) return(src);

	/*
	Interpolation is in color space so output image will an
	RGB image regardless of input format.
	*/

	if (dst == (Raster *) NULL) {
		dst = RasterCreate(nx, ny, RAS_DIRECT);
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
	x1v = (int *) malloc(dst->nx*sizeof(int));
	fracxv = (float *) malloc(dst->nx*sizeof(float));

	for(dx=0; dx<dst->nx; dx++) {
		fx = (float)dx*(((float)src->nx - 1)/((float)dst->nx - 1));
		x1v[dx] = floor(fx);
		fracxv[dx] = fx - x1v[dx];
	}

	for(dy=0; dy<dst->ny; dy++) {
		fy = dy * (((float)src->ny - 1)/((float)dst->ny - 1));
		y1 = floor(fy);
		y2 = y1 + 1;
		fracy  = fy - y1;
		if (verbose && dy % 50 == 0) {
			(void) fprintf(stderr, "Reconstructing row %d\n", dy);
		}
		for(dx=0; dx<dst->nx; dx++) {
			x1 = x1v[dx];
			x2 = x1 + 1;
			fracx  = fracxv[dx];

			rgbptr11 = &src->data[y1*3*src->nx + x1*3];
			rgbptr21 = rgbptr11 + 3;
			rgbptr12 = &src->data[y2*3*src->nx + x1*3];
			rgbptr22 = rgbptr12 + 3;

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

	(void) free((char *) x1v);
	(void) free((char *) fracxv);
	return(dst);
}
