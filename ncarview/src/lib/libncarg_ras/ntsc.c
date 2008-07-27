/*
 *	$Id: ntsc.c,v 1.8 2008-07-27 03:18:46 haley Exp $
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
#include <math.h>
#include "ncarg_ras.h"
#include "ntsc.h"

static double yiq_matrix[3][3] = {
	0.2989,  0.5866,  0.1144,
	0.5959, -0.2741, -0.3218,
	0.2113, -0.5227,  0.3113
};

static double tab[3][3][256];

static int	YIQBuildTable();
int		RasterYIQfilter();

RasterYIQfilter(src, dst)
	Raster		*src, *dst;
{
	static		init = 0;
	int		x, y, p;
	unsigned char	r, g, b;
	/*float		Y;*/
	float		I, Q;
	float		chroma_mag_sq, max_chroma_mag_sq;
	float		f;
	unsigned char	red[256], green[256], blue[256];

	if (dst->type != RAS_DIRECT) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterYIQfilter(src,dst) - %s",
			"dst type must be RAS_DIRECT");
		return(RAS_ERROR);
	}

	if (src->nx != dst->nx || src->ny != dst->ny) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterYIQfilter(src,dst) - %s",
			"src and dst resolutions are different");
		return(RAS_ERROR);
	}

	if (!init) {
		/*
		This saves a lot of calculation - YIQ mapping is precalculated.
		*/

		(void) YIQBuildTable();
	}

	max_chroma_mag_sq = (ChromaMax - Pedestal) / (LumaMax - Pedestal);
	max_chroma_mag_sq *= max_chroma_mag_sq;

	if (src->type == RAS_INDEXED) {
		for(p=0; p<src->ncolor; p++) {
			r = INDEXED_RED(src, p);
			g = INDEXED_GREEN(src, p);
			b = INDEXED_BLUE(src, p);

			/* Y is not yet used. */
			/* Y = tab[0][0][r] + tab[0][1][g] + tab[0][2][b]; */
			I = tab[1][0][r] + tab[1][1][g] + tab[1][2][b];
			Q = tab[2][0][r] + tab[2][1][g] + tab[2][2][b];

			chroma_mag_sq = I*I + Q*Q;

			if ( chroma_mag_sq > max_chroma_mag_sq) {
				f = sqrt(max_chroma_mag_sq)/sqrt(chroma_mag_sq);
				red[p]   = (int) (f * r);
				green[p] = (int) (f * g);
				blue[p]  = (int) (f * b);
			}
		}

		for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
			p = INDEXED_PIXEL(src, x, y);
			DIRECT_RED(dst, x, y)   = red[p];
			DIRECT_GREEN(dst, x, y) = green[p];
			DIRECT_BLUE(dst, x, y)  = blue[p];
		}}
	}

	if (src->type == RAS_DIRECT) {
		for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
			r = DIRECT_RED(src, x, y);
			g = DIRECT_GREEN(src, x, y);
			b = DIRECT_BLUE(src, x, y);

			/* Y is not yet used. */
			/* Y = tab[0][0][r] + tab[0][1][g] + tab[0][2][b]; */
			I = tab[1][0][r] + tab[1][1][g] + tab[1][2][b];
			Q = tab[2][0][r] + tab[2][1][g] + tab[2][2][b];

			chroma_mag_sq = I*I + Q*Q;

			if ( chroma_mag_sq > max_chroma_mag_sq) {
				f = sqrt(max_chroma_mag_sq)/sqrt(chroma_mag_sq);
				DIRECT_RED(dst, x, y)   = (int) (f * r);
				DIRECT_GREEN(dst, x, y) = (int) (f * g);
				DIRECT_BLUE(dst, x, y)  = (int) (f * b);
			}
		
		}}
	}

	return(RAS_OK);
}

static int
YIQBuildTable()
{
	int		p;
	float		pmax;

	pmax = (float) (MAX_PIXELS - 1);

	for(p = 0; p < MAX_PIXELS; p++) {
		tab[0][0][p] = (float)p * yiq_matrix[0][0] / pmax;
		tab[0][1][p] = (float)p * yiq_matrix[0][1] / pmax;
		tab[0][2][p] = (float)p * yiq_matrix[0][2] / pmax;
		tab[1][0][p] = (float)p * yiq_matrix[1][0] / pmax;
		tab[1][1][p] = (float)p * yiq_matrix[1][1] / pmax;
		tab[1][2][p] = (float)p * yiq_matrix[1][2] / pmax;
		tab[2][0][p] = (float)p * yiq_matrix[2][0] / pmax;
		tab[2][1][p] = (float)p * yiq_matrix[2][1] / pmax;
		tab[2][2][p] = (float)p * yiq_matrix[2][2] / pmax;
	}
}

RasterVHS(src, dst)
	Raster		*src;
	Raster		*dst;
{
	unsigned char	p, p1, p2, p3;
	int		x, y;

	if (dst->type != RAS_DIRECT) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterVHS(src,dst) - %s",
			"dst type must be RAS_DIRECT");
		return(RAS_ERROR);
	}

	if (src->nx != dst->nx || src->ny != dst->ny) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterVHS(src,dst) - %s",
			"src and dst resolutions are different");
		return(RAS_ERROR);
	}

	if (src->type == RAS_INDEXED) {
		for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
		  if (x == 0 || x == (src->nx - 1)) {
		    p = INDEXED_PIXEL(src, x, y);
		    DIRECT_RED(dst, x, y) = INDEXED_RED(src, p);
		    DIRECT_GREEN(dst, x, y) = INDEXED_GREEN(src, p);
		    DIRECT_BLUE(dst, x, y) = INDEXED_BLUE(src, p);
		  }
		  else {
			p1 = INDEXED_PIXEL(src, x-1, y);
			p2 = INDEXED_PIXEL(src, x, y);
			p3 = INDEXED_PIXEL(src, x+1, y);
			DIRECT_RED(dst, x, y) = (int) (INDEXED_RED(src, p1) +
						INDEXED_RED(src, p2) +
						INDEXED_RED(src, p3)) / 3;
			DIRECT_GREEN(dst, x, y) = (int) (INDEXED_RED(src, p1) +
						INDEXED_GREEN(src, p2) +
						INDEXED_GREEN(src, p3)) / 3;
			DIRECT_BLUE(dst, x, y) = (int) (INDEXED_BLUE(src, p1) +
						INDEXED_BLUE(src, p2) +
						INDEXED_BLUE(src, p3)) / 3;
		  }
		}}
	}
	else if (src->type == RAS_DIRECT) {
		for(y=0; y<src->ny; y++) {
		for(x=0; x<src->nx; x++) {
		  if (x == 0 || x == (src->nx - 1)) {
		    DIRECT_RED(dst, x, y)   = DIRECT_RED(src, x, y);
		    DIRECT_GREEN(dst, x, y) = DIRECT_GREEN(src, x, y);
		    DIRECT_BLUE(dst, x, y)  = DIRECT_BLUE(src, x, y);
		  }
		  else {
		    DIRECT_RED(dst, x, y) = (int) (DIRECT_RED(src, x-1, y) +
					    DIRECT_RED(src, x, y) +
					    DIRECT_RED(src, x+1, y)) / 3;
		    DIRECT_GREEN(dst, x, y) = (int) (DIRECT_GREEN(src, x-1, y) +
					    DIRECT_GREEN(src, x, y) +
					    DIRECT_GREEN(src, x+1, y)) / 3;
		    DIRECT_BLUE(dst, x, y) = (int) (DIRECT_BLUE(src, x-1, y) +
					    DIRECT_BLUE(src, x, y) +
					    DIRECT_BLUE(src, x+1, y)) / 3;
		  }
		}}
	}
	return(RAS_OK);
}

RasterFIRfilter(src, dst, fir_coeff)
	Raster		*src;
	Raster		*dst;
	float		*fir_coeff;
{
	int		filter_length = 5;
	unsigned char	p;
	int		i, x, y;
	float		rsum, gsum, bsum;
	int		fx, delta;

	if (dst->type != RAS_DIRECT) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterFIRfilter(src,dst) - %s",
			"dst type must be RAS_DIRECT");
		return(RAS_ERROR);
	}

	if (src->nx != dst->nx || src->ny != dst->ny) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterFIRfilter(src,dst) - %s",
			"src and dst resolutions are different");
		return(RAS_ERROR);
	}

	if (filter_length%2 == 0) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"FIR must be odd length");
		return(RAS_ERROR);
	}

	delta = filter_length/2;

	for(y=0; y<src->ny; y++) {
	for(x=0; x<src->nx; x++) {

		if (x < delta || x > src->nx - delta - 1) {
			if (src->type == RAS_INDEXED) {
			  p = INDEXED_PIXEL(src, x, y);
			  DIRECT_RED(dst, x, y) = INDEXED_RED(src,p);
			  DIRECT_GREEN(dst, x, y) = INDEXED_GREEN(src,p);
			  DIRECT_BLUE(dst, x, y) = INDEXED_BLUE(src,p);
			}
			else if (src->type == RAS_DIRECT) {
			  DIRECT_RED(dst, x, y) = DIRECT_RED(src, x, y);
			  DIRECT_GREEN(dst, x, y) = DIRECT_GREEN(src, x, y);
			  DIRECT_BLUE(dst, x, y) = DIRECT_BLUE(src, x, y);
			}
		}
		else {
			rsum = gsum = bsum = 0.;

			for(i=0, fx = x - delta; fx <= x + delta; fx++,i++) {
			  if (src->type == RAS_INDEXED) {
			    p = INDEXED_PIXEL(src, fx, y);
			    rsum += (float)INDEXED_RED(src, p)   *fir_coeff[i];
			    gsum += (float)INDEXED_GREEN(src, p) *fir_coeff[i];
			    bsum += (float)INDEXED_BLUE(src, p)  *fir_coeff[i];
			  }
			  else if (src->type == RAS_DIRECT) {
			    rsum+=(float)DIRECT_RED(src, fx, y)  *fir_coeff[i];
			    gsum+=(float)DIRECT_GREEN(src, fx, y)*fir_coeff[i];
			    bsum+=(float)DIRECT_BLUE(src, fx, y) *fir_coeff[i];
			  }
			}

			DIRECT_RED(dst, x, y)   = (unsigned char) rsum;
			DIRECT_GREEN(dst, x, y) = (unsigned char) gsum;
			DIRECT_BLUE(dst, x, y)  = (unsigned char) bsum;
		}
	}}
	return(RAS_OK);
}
