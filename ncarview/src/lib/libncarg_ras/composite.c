/*
 *	$Id: composite.c,v 1.8 2008-07-27 03:18:46 haley Exp $
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
#include "ncarg_ras.h"

/**********************************************************************
 *	Function: RasterDissolve(a, b, dst, alpha1)
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
 *			dst = a * alpha1 + b * (1 - alpha1)
 *		
 *		If alpha1 is 1.0, "dst" is "a". If alpha1 is 0.0,
 *		"dst" is "b".
 *
 *		"a" and "b" can be Raster* structures of any
 *		type. "dst" must be a preallocated (Raster *)
 *		structure of type RAS_DIRECT. "alpha1" is a float.
 *		
 *********************************************************************/
int
RasterDissolve(a, b, dst, alpha1)
	Raster		*a, *b, *dst;
	float		alpha1;
{
	int		p, nx, ny, x, y;
	RasterEncoding	atype, btype;
	unsigned char	ra, ga, ba, rb, gb, bb;
	float		p_alpha1[256], p_alpha1_one[256];

	if (a->nx != b->nx || a->ny != b->ny) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterDissolve() - a & b have different resolutions");
		return(RAS_ERROR);
	}

	if (dst->type != RAS_DIRECT) {
		(void) ESprintf(RAS_E_PROGRAMMING,
			"RasterDissolve() - dst must be DIRECT color");
		return(RAS_ERROR);
	}

	nx = a->nx ; ny = b->ny;
	atype = a->type; btype = b->type;

	for(p=0; p<256; p++) {
		p_alpha1[p] = (float) p * alpha1;
		p_alpha1_one[p] = (float) p * (1.0 - alpha1);
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
					p_alpha1[ra] + p_alpha1_one[rb];
		DIRECT_GREEN(dst, x, y) = (unsigned char) 
					p_alpha1[ga] + p_alpha1_one[gb];
		DIRECT_BLUE(dst, x, y) = (unsigned char)
					p_alpha1[ba] + p_alpha1_one[bb];
	}}
	return(RAS_OK);
}
