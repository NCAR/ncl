/*
 *	$Id: clip.c,v 1.8 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*
 *	Author:	Tinsley Galyean (tag@boulder.colorado.edu)
 *
 *	Date:	Thu Mar 10 15:15:44 MST 1988
 *
 * rev 1.01 clyne 3/09/90	: added gcap_set_clip to turn clipping off
 *				  and on 
 */
#include <stdio.h>
#include <ncarg/c.h>
#include "default.h"
#include "translate.h"
#include "ctrandef.h"


long	clipxmin;
long	clipymin;
long	clipxmax;
long	clipymax;

/*
 *	gcap_set_clip
 *	[exported]
 *
 *	set the clip rectangle. If clip_on is set than set clipping 
 *	rectangle to max_clip_rect the union of the smallest rectangle defined
 *	by max_clip_rect and CLIP*(MAX,MIN). Else set it to the  smallest 
 *	rectangle that is the union of max_clip_rect and clip_rect
 */
gcap_set_clip(max_clip_rect, clip_rect, clip_on)
	CoordRect	max_clip_rect;
	CoordRect	clip_rect;
	boolean		clip_on;
{
	if (clip_on) {
		clipxmin = MAX(max_clip_rect.llx, CLIPXMIN);
		clipymin = MAX(max_clip_rect.lly, CLIPYMIN);
		clipxmax = MIN(max_clip_rect.urx, CLIPXMAX);
		clipymax = MIN(max_clip_rect.ury, CLIPYMAX);
	}
	else {
		/*
		 * clip to the smallest rectangle defined by the union of 
		 * dev_coord and win_coord
		 */
		clipxmin = MAX(max_clip_rect.llx, clip_rect.llx);
		clipymin = MAX(max_clip_rect.lly, clip_rect.lly);
		clipxmax = MIN(max_clip_rect.urx, clip_rect.urx);
		clipymax = MIN(max_clip_rect.ury, clip_rect.ury);
	}
}
	

/*
 *	This routine gets the clipping bounds from the default table.
 *
 *	Return code is  0 -- if line is completely out of the clip area
 *			1 -- if the line has a visible section
 *			2 -- if no clipping was needed
 *
 *	This algorithm was taken from Foley and VanDam
 */
int Clipper(ox1,oy1,ox2,oy2,x1,y1,x2,y2)
long	ox1,oy1,ox2,oy2; 		/* the old points */
register long	*x1,*y1,*x2,*y2;	/* the new points */
{
	int		i;	/* looping variable */

	long		ltmp;
	boolean		btmp;
	boolean		swap = FALSE;

	boolean		outcode1[4],outcode2[4];



	*x1=ox1; *y1=oy1; *x2=ox2; *y2=oy2;
	
	while (1) {
		outcode1[0] = (*y1 > clipymax) ? TRUE : FALSE;
		outcode1[1] = (*y1 < clipymin) ? TRUE : FALSE;
		outcode1[2] = (*x1 > clipxmax) ? TRUE : FALSE;
		outcode1[3] = (*x1 < clipxmin) ? TRUE : FALSE;

		outcode2[0] = (*y2 > clipymax) ? TRUE : FALSE;
		outcode2[1] = (*y2 < clipymin) ? TRUE : FALSE;
		outcode2[2] = (*x2 > clipxmax) ? TRUE : FALSE;
		outcode2[3] = (*x2 < clipxmin) ? TRUE : FALSE;

		/* trivial reject */
		if ((outcode1[0] && outcode2[0]) ||
		    (outcode1[1] && outcode2[1]) ||
		    (outcode1[2] && outcode2[2]) ||
		    (outcode1[3] && outcode2[3]))
			return (FALSE);

		/* trivial accept */
		if (!(outcode1[0]) && !(outcode2[0]) &&
		    !(outcode1[1]) && !(outcode2[1]) &&
		    !(outcode1[2]) && !(outcode2[2]) &&
		    !(outcode1[3]) && !(outcode2[3])) {
			return (TRUE);
		}

		/* subdivide the line since at most only one point is
		 * in the window.
		 */

		/* Assure that P1 is outside the window */
		if (!(outcode1[0] || outcode1[1] || outcode1[2] || outcode1[3])) {
			ltmp = *x1;*x1 = *x2;*x2 = ltmp;
			ltmp = *y1;*y1 = *y2;*y2 = ltmp;
			for (i=0;i<4;i++) {
				btmp = outcode1[i];
				outcode1[i] = outcode2[i];
				outcode2[i] = btmp;
			}
			swap = 1;
		}

		/* move P1 to the intersection point */

		if (outcode1[0]) {
			*x1= *x1 + (*x2 - *x1) * (clipymax - *y1) / (*y2 - *y1);
			*y1= clipymax;
		}
		else if (outcode1[1]) {
			*x1= *x1 + (*x2 - *x1) * (clipymin - *y1) / (*y2 - *y1);
			*y1= clipymin;
		}
		else if (outcode1[2]) {
			*y1= *y1 + (*y2 - *y1) * (clipxmax - *x1) / (*x2 - *x1);
			*x1= clipxmax;
		}
		else if (outcode1[3]) {
			*y1= *y1 + (*y2 - *y1) * (clipxmin - *x1) / (*x2 - *x1);
			*x1= clipxmin;
		}

		if (swap) {
			ltmp = *x1;*x1 = *x2;*x2 = ltmp;
			ltmp = *y1;*y1 = *y2;*y2 = ltmp;
		}
	}

}
