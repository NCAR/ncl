#include "ncarg_ras.h"

extern int	OptionDitherPopular;
extern int	OptionDitherBits;
extern int	OptionDitherColors;

#define MAXBITS		6
#define MAXBINS		(1 << MAXBITS)

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
		(void) RasterSetError(RAS_E_BOGUS_RASTER_STRUCTURE);
		return(RAS_ERROR);
	}

	for(i=0; i<256; i++) {
		dst->red[i]   = ((i & 0007) >> 0) * 32;
		dst->green[i] = ((i & 0070) >> 3) * 32;
		dst->blue[i]  = ((i & 0300) >> 6) * 64;
	}

	for(y=0; y<src->ny; y++) {
		if (y%50 == 0 && verbose) 
			fprintf(stderr, "Dithering row %4d\n", y);

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
RasterDitherPopular(src, dst, verbose)
	Raster	*src;
	Raster	*dst;
	int	verbose;
{
	int		i, j;
	int		x, y;
	unsigned char	r, g, b;
	int		dist, mini, mindist;
	int		bins, shift;
	int		count;
	int		cmapsize, qbits;
	int		hist[MAXBINS][MAXBINS][MAXBINS];
	int		chist[256];

	if (src == (Raster *) NULL || dst == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_BOGUS_RASTER_STRUCTURE);
		return(RAS_ERROR);
	}

	cmapsize = OptionDitherColors;
	qbits = OptionDitherBits;
	bins = 1 << qbits;
	shift = 8 - qbits;

	if (qbits > MAXBITS) {
		(void) RasterSetError(RAS_E_TOO_MANY_DITHERBITS);
		return(RAS_ERROR);
	}

	if (verbose) {
		fprintf(stderr, "Popularity Dithering with %d bits\n", qbits);
	}

	/* Initialize the colormap histogram */
	if (verbose) fprintf(stderr, "Initializing...");
	for(i=0; i<256; i++) {
		chist[i] = 0;
		dst->red[i] = 0;
		dst->green[i] = 0;
		dst->blue[i] = 0;
	}
		
	for(r=0; r<bins; r++) for(g=0; g<bins; g++) for(b=0; b<bins; b++) {
		hist[r][g][b] = 0;
	}
	if (verbose) fprintf(stderr, "Done\n");

	if (verbose) fprintf(stderr, "Calculating histogram...");
	for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
			r = DIRECT_RED(src, x, y)   >> shift;
			g = DIRECT_GREEN(src, x, y) >> shift;
			b = DIRECT_BLUE(src, x, y)  >> shift;
			hist[r][g][b]++;
		}
	}
	if (verbose) fprintf(stderr, "Done\n");

	if (verbose) fprintf(stderr, "Building new colormap...");
	for(r=0;r<bins;r++) {
	if (verbose) fprintf(stderr, "red = %d\n", r);
	for(g=0;g<bins;g++) for(b=0;b<bins;b++) {
		count = hist[r][g][b];
		if (count == 0) continue;
			
		for(i=0; i<256; i++) {
			if (count > chist[i]) {
				for(j=254; j>=i; j--) {
					chist[j+1] = chist[j];
					dst->red[j+1]   = dst->red[j];
					dst->green[j+1] = dst->green[j];
					dst->blue[j+1]  = dst->blue[j];
				}
				chist[i] = count;
				dst->red[i] = r;
				dst->green[i] = g;
				dst->blue[i] = b;
				break;
			}
		}
	}
	}
	if (verbose) fprintf(stderr, "Done\n");

	/* Reuse the RGB histogram as an associative memory */

	if (verbose) fprintf(stderr, "Encoding color table...");
	for(i=0; i<256; i++) {
		hist[dst->red[i]][dst->green[i]][dst->blue[i]] = -1 - i;
		dst->red[i] = dst->red[i] << shift;
		dst->green[i] = dst->green[i] << shift;
		dst->blue[i] = dst->blue[i] << shift;
	}
	if (verbose) fprintf(stderr, "Done\n");
		
	if (verbose) fprintf(stderr, "Remapping the image...\n");
	for(y=0; y<src->ny; y++) {
		if (y%50==0 && verbose) fprintf(stderr,"Remapping row %d\n",y);
		for(x=0; x<src->nx; x++) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);
			count = hist[r>>shift][g>>shift][b>>shift];
			if (count < 0) {
				INDEXED_PIXEL(dst, x, y) = abs(count+1);
			}
			else {
				mindist = 1000; mini = -1;
				for(i=0; i<256; i++) {
					dist = abs(r - dst->red[i]) +
					       abs(g - dst->green[i]) +
					       abs(b - dst->blue[i]);
					if (dist < mindist) {
						mindist = dist;
						mini = i;
					}
				}
				INDEXED_PIXEL(dst, x, y) = mini;
			}
		}
	}
	if (verbose) fprintf(stderr, "Done\n");

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
