/*
 *	$Id: color.c,v 1.4 2008-07-27 03:18:46 haley Exp $
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
/*	File:	color.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/26/92
 *
 *	Description:
 *		This file contains functions that operate
 *		on general image color properties. Color
 *		quanitization is handled in another file.
 *
 *	Disclaimer:
 *		Author is color deficient ("color challenged", for
 *		the politically correct), with the major axis of
 *		deficiency being green. This could be reflected in
 *		the code.
 *
 *		
 */
#include	<stdio.h>
#include	<string.h>
#include	<math.h>
#include	"ncarg_ras.h"

RasterRGBScale(src, scale)
	Raster	*src;
	float	scale;
{
	int	i;
	int	x, y;

	if (scale == 1.0) return(RAS_OK);

	if (src->type == RAS_INDEXED) {
		for(i=0; i<256; i++) {
			src->red[i]   = (int) ( (float) src->red[i] * scale);
			src->green[i] = (int) ( (float) src->green[i] * scale);
			src->blue[i]  = (int) ( (float) src->blue[i] * scale);
		}
	}
	else if (src->type == RAS_DIRECT) {
		for(y=0; y<src->ny; y++)
		for(x=0; x<src->nx; x++) {
			DIRECT_RED(src, x, y) = (int) 
				( (float) DIRECT_RED(src, x, y) * scale);
			DIRECT_GREEN(src, x, y) = (int) 
				( (float) DIRECT_GREEN(src, x, y) * scale);
			DIRECT_BLUE(src, x, y) = (int) 
				( (float) DIRECT_BLUE(src, x, y) * scale);
		}
	}

	return(RAS_OK);
}
