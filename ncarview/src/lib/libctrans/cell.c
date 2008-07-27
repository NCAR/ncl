/*
 *	$Id: cell.c,v 1.8 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1992                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	cell.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Jul  5 14:19:29 MDT 1990
 *
 *	Massage cell array data necessary for all devices
 */

#include "cgmc.h"
#include "ctrandef.h"


/*
 *	SetUpCellArrayAddressing
 *	[internal]
 *
 *	calulate addressing information for manipulating ximage data for
 *	rendering a cell array. This function assumes the corner points
 *	P,Q,R are as described in the ANSI standard for CGM.
 *
 * on entry
 *	P,Q,R		: cell array corners
 *	image_size	: size of image data in bytes
 *	pad		: number of bytes of padding at end of a scan line
 *	bpp		: size of pixel in bytes
 *	bpl		: number of bytes per line in the ximage
 *	xoff, yoff	: offset of origin in destination image
 * on exit
 *	*step_x		: increment in x direction
 *	*step_y		: increment in y direction
 *	*start_x	: x destination of ximage in drawable
 *	*start_y	: y destination of ximage in drawable
 *	*data		: initialized to proper starting address
 */
void	SetUpCellArrayAddressing(Pcoord, Qcoord, Rcoord, image_size, pad, 
		bpp, bpl, xoff, yoff, step_x, step_y, start_x, start_y, data)

	Ptype		Pcoord, Qcoord, Rcoord;
	unsigned int	image_size,
			pad,
			bpp,
			bpl;
	int		xoff, yoff;
	int		*step_x,
			*step_y;
	int		*start_x,
			*start_y;
	char		**data;
{

	/*
	 * determine addressing information based on relative positions of 
	 * P, Q and R.
	 */

	*data += (yoff * bpl) + xoff;

	if (Pcoord.x < Rcoord.x) {
		*start_x = Pcoord.x;
		*step_x = bpp;
		if (Rcoord.y < Qcoord.y) {
			*start_y = Rcoord.y;
			*step_y = bpl;
			*data += 0;
		}
		else {
			*start_y = Qcoord.y;
			*step_y = -bpl;
			*data += (image_size - bpl);
		}
	}
	else {
		*start_x = Rcoord.x;
		*step_x = -bpp;
		if (Rcoord.y < Qcoord.y) {
			*start_y = Rcoord.y;
			*step_y = bpl;
			*data += (bpl - pad - bpp);
		}
		else {
			*start_y = Qcoord.y;
			*step_y = -bpl;
			*data += (image_size - (bpp + pad));
		}
	}
}



/*
 *	SetUpCellArrayIndexing
 *	[internal]
 *
 *	Perform interpolation of Cell array data necessitated by imperfect
 *	mapping of cell array cells onto device pixels.
 *
 * on entry
 *	image_height	: height of image in pixels
 *	image_width	: width of image in pixels
 *	*rows		: Has dimension ny
 *	*cols		: Has dimension nx
 *	nx		: number of cells per row
 *	ny		: number of cells per column
 * on exit
 *	*rows		: row[i] specifies number of pixels for a cell in row i
 *	*cols		: col[i] specifies number of pixels for a cell in col i
 */
void	SetUpCellArrayIndexing(image_width, image_height, rows, cols, nx, ny)
	unsigned int	image_width,
			image_height;
	int		*rows, 
			*cols;
	unsigned	nx, ny;
{
	int	i;
	int	left, right;
	double	inc, tmp;

	/*
	 * map cell array onto available pixels. Use current IEEE
	 * rounding rules to determine whether a cell boundry includes
	 * a boundry pixel or not. rows[i] and cols[j] contain
	 * the number of pixels that make up cell[i,j] 
	 */
	inc = (double) image_width / (double) nx;
	for( right = 0, tmp = 0.0,i = 0; i < nx; i++) {	/* map cols*/
		left = right;
		tmp += inc;
		right =  (int) ROUND(tmp);
		cols[i] = right - left;
	}

	inc = (double) image_height / (double) ny;
	for( right = 0, tmp = 0.0,i = 0; i < ny; i++) {	/* map rows*/
		left = right;
		tmp += inc;
		right =  (int) ROUND(tmp);
		rows[i] = right - left;
	}
}
