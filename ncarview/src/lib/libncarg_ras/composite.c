#include <stdio.h>
#include "ncarg_ras.h"

/**********************************************************************
 *	Function: RasterDissolve(a, b, dst, alpha)
 *
 *	Author:	Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	January 1992
 *
 *	Description:
 *		RasterDissolve combines two images according to the
 *		standard dissolve equation:
 *
 *			dst = a * alpha + b * (1 - alpha)
 *		
 *		If alpha is 1.0, "dst" is "a". If alpha is 0.0,
 *		"dst" is "b".
 *
 *		"a" and "b" can be Raster* structures of any
 *		type. "dst" must be a preallocated (Raster *)
 *		structure of type RAS_DIRECT. "alpha" is a float.
 *		
 *********************************************************************/
int
RasterDissolve(a, b, dst, alpha)
	Raster		*a, *b, *dst;
	float		alpha;
{
	int		p, nx, ny, x, y;
	RasterEncoding	atype, btype;
	unsigned char	ra, ga, ba, rb, gb, bb;
	float		p_alpha[256], p_alpha_one[256];

	if (a->nx != b->nx || a->ny != b->ny) {
		(void) RasterSetError(RAS_E_UNSUPPORTED_RESOLUTION);
		return(RAS_ERROR);
	}

	if (dst->type != RAS_DIRECT) {
		(void) RasterSetError(RAS_E_INTERNAL_PROGRAMMING);
		return(RAS_ERROR);
	}

	nx = a->nx ; ny = b->ny;
	atype = a->type; btype = b->type;

	for(p=0; p<256; p++) {
		p_alpha[p] = (float) p * alpha;
		p_alpha_one[p] = (float) p * (1.0 - alpha);
	}

	for(y = 0; y < ny; y++) {
	for(x = 0; x < nx; x++) {
		if (atype == RAS_DIRECT) {
			ra = DIRECT_RED(a, x, y);
			ga = DIRECT_GREEN(a, x, y);
			ba = DIRECT_BLUE(a, x, y);
		}
		else if (atype == RAS_INDEXED) {
			p = INDEXED_PIXEL(a, x, y);
			ra = INDEXED_RED(a, p);
			ga = INDEXED_GREEN(a, p);
			ba = INDEXED_BLUE(a, p);
		}

		if (btype == RAS_DIRECT) {
			rb = DIRECT_RED(b, x, y);
			gb = DIRECT_GREEN(b, x, y);
			bb = DIRECT_BLUE(b, x, y);
		}
		else if (atype == RAS_INDEXED) {
			p = INDEXED_PIXEL(b, x, y);
			rb = INDEXED_RED(b, p);
			gb = INDEXED_GREEN(b, p);
			bb = INDEXED_BLUE(b, p);
		}

		DIRECT_RED(dst, x, y) = (unsigned char)
					p_alpha[ra] + p_alpha_one[rb];
		DIRECT_GREEN(dst, x, y) = (unsigned char) 
					p_alpha[ga] + p_alpha_one[gb];
		DIRECT_BLUE(dst, x, y) = (unsigned char)
					p_alpha[ba] + p_alpha_one[bb];
	}}
	return(RAS_OK);
}
