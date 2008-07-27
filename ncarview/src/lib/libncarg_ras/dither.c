/*
 *	$Id: dither.c,v 1.10 2008-07-27 03:18:46 haley Exp $
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

#include <stdlib.h>
#include "options.h"
#include "ncarg_ras.h"

#define MAXBITS		6
#define MAXBINS		(1 << MAXBITS)

/*LINTLIBRARY*/

typedef	struct CMapEntry_ {
		int		index;
		unsigned char	r,g,b;
	} CMapEntry;

static int	compare_rgb(const void *ptr1, const void *ptr2)
{
	CMapEntry  *ce1ptr = (CMapEntry *) ptr1;
	CMapEntry  *ce2ptr = (CMapEntry *) ptr2;
	int	i1, i2;

	i1 = ce1ptr->r + ce1ptr->g + ce1ptr->b;
	i2 = ce2ptr->r + ce2ptr->g + ce2ptr->b;

	if (i1 < i2) return(-1);
	if (i1 > i2) return (1);
	return(0);
}

int
RasterDither(src, dst, verbose)
	Raster	*src;
	Raster	*dst;
	int	verbose;
{
	int	i;
	int	x, y;
	int	r, g, b;
	int	pixel;

	if (OptionDitherPopular) {
		return(RasterDitherPopular(src,dst,verbose));
	}

	if (src == (Raster *) NULL || dst == (Raster *) NULL) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterDither() - NULL argument");
		return(RAS_ERROR);
	}

	for(i=0; i<256; i++) {
		dst->red[i]   = ((i & 0007) >> 0) * 32;
		dst->green[i] = ((i & 0070) >> 3) * 32;
		dst->blue[i]  = ((i & 0300) >> 6) * 64;
	}

	for(y=0; y<src->ny; y++) {
		if (y%50 == 0 && verbose) 
			(void) fprintf(stderr, "Dithering row %4d\n", y);

		for(x=0; x<src->nx; x++) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);

			pixel = ((r & 0xe0) >> 5) |
				((g & 0xe0) >> 2) |
				((b & 0xc0)) ;
			INDEXED_PIXEL(dst, x, y) = pixel;
		}
	}
	return(RAS_OK);
}

int
Raster8to24bit(src, dst, verbose)
	Raster	*src;
	Raster	*dst;
	int	verbose;
{
	int	i;
	int	x, y;
	int	r, g, b;
	int	pixel;


	if (src == (Raster *) NULL || dst == (Raster *) NULL) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterDither() - NULL argument");
		return(RAS_ERROR);
	}

	for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
			pixel = INDEXED_PIXEL(src, x, y);

			DIRECT_RED(dst, x, y) = src->red[pixel];
			DIRECT_GREEN(dst, x, y) = src->green[pixel];
			DIRECT_BLUE(dst, x, y) = src->blue[pixel];

		}
	}
	return(RAS_OK);
}

int
RasterDitherPopular(src, dst, verbose)
	Raster	*src;
	Raster	*dst;
	int	verbose;
{
	int		i, j;
	int		x, y;
	unsigned char	r, g, b, bins;
	int		dist, mini, mindist;
	int		shift;
	int		count;
	int		cmapsize;
	int		hist[MAXBINS][MAXBINS][MAXBINS];
	int		chist[256];
	int		imap[256];
	int		orig_colors;
	CMapEntry	sorted_cmap[256];

	if (src == (Raster *) NULL || dst == (Raster *) NULL) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterDitherPopular() - NULL argument");
		return(RAS_ERROR);
	}

	if (OptionDitherBits > MAXBITS) {
		(void) ESprintf(RAS_E_TOO_MANY_DITHERBITS,
			"RasterDitherPopular()");
		return(RAS_ERROR);
	}

	cmapsize = OptionDitherColors;
	bins = 1 << OptionDitherBits;
	shift = 8 - OptionDitherBits;

	if (verbose) {
		(void) fprintf(stderr, 
			"Popularity Dithering with %d bits\n",OptionDitherBits);
	}

	/* Initialize the colormap histogram */

	if (verbose) (void) fprintf(stderr, "Initializing...");
	for(i=0; i<256; i++) {
		chist[i] = 0;
		dst->red[i] = 0;
		dst->green[i] = 0;
		dst->blue[i] = 0;
	}

	/* Zero the rgb histogram. */
		
	for(r=0; r<bins; r++) for(g=0; g<bins; g++) for(b=0; b<bins; b++) {
		hist[r][g][b] = 0;
	}
	if (verbose) (void) fprintf(stderr, "Done\n");

	/* Calculate the histogram. */

	if (verbose) (void) fprintf(stderr, "Calculating histogram...");
	for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
			r = DIRECT_RED(src, x, y)   >> shift;
			g = DIRECT_GREEN(src, x, y) >> shift;
			b = DIRECT_BLUE(src, x, y)  >> shift;
			hist[r][g][b]++;
		}
	}
	if (verbose) (void) fprintf(stderr, "Done\n");

	/* Pick the most popular colors and place them in the colormap */

	if (verbose) (void) fprintf(stderr, "Building new colormap...");
	orig_colors = 0;

	for(r=0;r<bins;r++) {
	for(g=0;g<bins;g++) {
	for(b=0;b<bins;b++) {
		if (hist[r][g][b] > 0) {
			orig_colors++;
		}

		count = hist[r][g][b];
		if (count == 0) continue;
			
		for(i=0; i<cmapsize; i++) {
			if (count > chist[i]) {
				for(j=cmapsize-2; j >= i; j--) {
					dst->red[j+1]   = dst->red[j];
					dst->green[j+1] = dst->green[j];
					dst->blue[j+1]  = dst->blue[j];
					chist[j+1] = chist[j];
				}
				chist[i] = count;
				dst->red[i] = r;
				dst->green[i] = g;
				dst->blue[i] = b;
				break;
			}
		}
	}}}
	if (verbose) {
		(void) fprintf(stderr, "Done\n");
		(void) fprintf(stderr, 
			"There were %d unique colors in original image\n", 
			orig_colors);
	}

	/*
	Record in the RGB histogram when a cell *is* one of the color 
	table entries. Other colors will have to be matched to the "best" cell.
	*/

	if (verbose) (void) fprintf(stderr, "Encoding color table...");
	for(i=0; i<cmapsize; i++) {
		hist[dst->red[i]][dst->green[i]][dst->blue[i]] = -1 - i;
		sorted_cmap[i].r = dst->red[i] = dst->red[i] << shift;
		sorted_cmap[i].g = dst->green[i] = dst->green[i] << shift;
		sorted_cmap[i].b = dst->blue[i] = dst->blue[i] << shift;
		sorted_cmap[i].index = i;
	}

	/*
	 * sort the color map into a sorted color map
	 */
	qsort(sorted_cmap, cmapsize, sizeof(sorted_cmap[0]), compare_rgb);
	for(i=0; i<cmapsize; i++) {
		imap[sorted_cmap[i].index] = i;
	}

	if (verbose) (void) fprintf(stderr, "Done\n");

	/*
	Remap the original true color image into the new compressed
	color space. Input pixels that are exact matches are recognized
	early; others are matched with a color table entry based on
	distance in RGB space.
	*/

		
	if (verbose) (void) fprintf(stderr, "Remapping the image...\n");
	for(y=0; y<src->ny; y++) {
		if (y%50==0 && verbose) {
			(void) fprintf(stderr,"Remapping row %d\n",y);
		}

		for(x=0; x<src->nx; x++) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);
			count = hist[r>>shift][g>>shift][b>>shift];
			if (count < 0) {
				INDEXED_PIXEL(dst, x, y) = imap[abs(count+1)];
			}
			else {
				mindist = 1000; mini = -1;
				for(i=0; i<cmapsize; i++) {
					dist = abs((int) (r - dst->red[i])) +
					       abs((int) (g - dst->green[i])) +
					       abs((int) (b - dst->blue[i]));
					if (dist < mindist) {
						mindist = dist;
						mini = i;
					}
				}
				INDEXED_PIXEL(dst, x, y) = imap[mini];
			}
		}
	}
	for(i=0; i<cmapsize; i++) {
		dst->red[i] = sorted_cmap[i].r;
		dst->green[i] = sorted_cmap[i].g;
		dst->blue[i] = sorted_cmap[i].b;
	}
	if (verbose) (void) fprintf(stderr, "Done\n");

	return(RAS_OK);
}

int
RasterColorToGray(src, dst)
	Raster	*src;
	Raster	*dst;
{
	int	i;
	int	x, y;
	int	r, g, b;
	int	pixel;
	float	luminance;

	for(i=0; i<256; i++) {
		dst->red[i]   = i;
		dst->green[i] = i;
		dst->blue[i]  = i;
	}

	for(y=0; y<src->ny; y++)
	for(x=0; x<src->nx; x++)
	{
		if (src->type == RAS_INDEXED) {
			pixel = INDEXED_PIXEL(src, x, y);
			r = INDEXED_RED(src, pixel);
			g = INDEXED_RED(src, pixel);
			b = INDEXED_RED(src, pixel);
		}
		else if (src->type == RAS_DIRECT) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);
		}

		luminance = 0.299 * r + 0.587 * g + 0.114 * b;
		INDEXED_PIXEL(dst, x, y) = (int) luminance;
	}
	return(RAS_OK);
}
