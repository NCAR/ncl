/*
 *	$Id: color.c,v 1.3 2000-08-22 15:12:09 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
