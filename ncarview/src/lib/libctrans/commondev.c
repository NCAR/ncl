/*
 *	$Id: commondev.c,v 1.31 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1994                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/c.h>
#include "commondev.h"
#include "gcapdev.h"
#include "rastdev.h"
#include "cgmc.h"
#include "soft_fill.h"
#include "default.h"
#include "translate.h"
#include "ctrandef.h"
#include "hatch.h"

static	ComDev	func_tab[] = {
	{
	"gcap", 	gcap_open,	gcap_close,	gcap_pointflush,
	gcap_line,	gcap_devline,	gcap_linestyle,	gcap_linecolour,
	gcap_fillcolour, gcap_linewidth, gcap_update_color_table
	},
	{
	"raster", 	rast_open,	rast_close,	rast_pointflush,
	rast_line,	rast_devline,	rast_linestyle,	rast_linecolour,
	rast_fillcolour, rast_linewidth, rast_update_color_table
	}
};
	
static	int	numDev = sizeof (func_tab) / sizeof (ComDev);

static	ComDev	*dev;

extern	boolean	*softFill;
extern	boolean	startedDrawing;

/*
 *      A buffer holding a lot of x,y pairs so that as the clipping is done
 *      we have a place to put them.  This is necessary because in some cases
 *      it will be necessary to know the number of coord pair before sending
 *      any of the pairs.
 */
static	Ptype	*coordBuf = NULL;
static	long	coordBufNum = 0;	/* the number of coords
						currently in the buffer */
static	long	coordBufSize = 0;       /* amount mem allocated         */
static	unsigned long	maxPolyPoints = ~(unsigned long) 0;

int	commFillScaleFactor = 1;
int	commHatchScaleFactor = 1;

#define	SEGMENTS	64
static	quick_circle(xc,yc,radius)
	int	xc, yc;
	int	radius;
{
	long		x1, y1;
	register long	x2, y2;
	int		i;
	double		inc = (2.0 * (double) M_PI) / (double) SEGMENTS;	
	register double	theta = 0.0;

	x2 = ((long) ((float) radius * cos(theta))) + xc;
	y2 = ((long) ((float) radius * sin(theta))) + yc;;

	for (i = 0; i < SEGMENTS; i++) {
		theta += inc;
		x1 = x2;
		y1 = y2;
		x2 = ((long) ((float) radius * cos(theta))) + xc;
		y2 = ((long) ((float) radius * sin(theta))) + yc;;
		dev->line(x1, y1, x2, y2);
	}
}

static	void	fat_segment(x1, y1, x2, y2, dx, dy, line_width) 
	VDCtype	x1, y1, x2, y2;
	VDCtype	dx, dy;
	int	line_width;
{

	int	num_lines;
	int	i;

	if (line_width == 1) return;

	dx /= 2;
	dy /= 2;

	
	/*
	 * move starting point to first scan line.
	 */
	x1 = x1 - ((line_width - 1) * dx);
	y1 = y1 - ((line_width - 1) * dy);
	x2 = x2 - ((line_width - 1) * dx);
	y2 = y2 - ((line_width - 1) * dy);

	/*
	 * how many lines we need to draw
	 */
	num_lines = (2 * line_width) - 1;

	for (i = 0; i < num_lines; i++) {
		dev->line(x1, y1, x2, y2);

		x1 += dx; y1 += dy;
		x2 += dx; y2 += dy;
	}
}

ComSetDevice(name)
	char	*name;
{
	char	*s;
	int	i;

	if (strcmp("gcap", name) == 0) s = name;
	else s = "raster"; 

	for (i=0; i < numDev; i++) {
		if (strcmp(s, func_tab[i].name) == 0) {
			dev = &func_tab[i];
			break;
		}
	}

	init_common();

	dev->open(&maxPolyPoints);
}

ComClose()
{
	if (coordBuf) free ((Voidptr) coordBuf);
	coordBuf = NULL;

	dev->close();
}

init_common()
{

	/*
	 * allocate memory for coordinate buffer
	 */
	if (coordBuf) free ((Voidptr) coordBuf);
	coordBuf = (Ptype *) malloc (1024 * (unsigned) sizeof(Ptype));
	coordBufSize = 1024;

}


/*
 *	sim_polygon
 *	use software to simulate a filled polygon
 */
void	ComSimPoly(p_list, n, skip)
	Ptype	*p_list;
	int	n;
	int	skip;
{
	FillTable       *fill_table;
	int     i;
	DCtype	yval;

	if (n < 2)
		return;
	/*
	 * check and set color. This is complicated since we are using
	 * lines to fill polygons.
	 */
	if (FILL_COLOUR_DAMAGE) {
		dev->setlinecolour(FILL_COLOUR.index);/* set LINE color */
		LINE_COLOUR_DAMAGE = TRUE;
	}

	/*
	 * convert VDC coordinates to DC (device) coordinates
	 * (note this only legal since VDCtype and DCtype are the same type)
	 */
	for (i = 0; i < n; i++) {
		p_list[i].x = XConvert(p_list[i].x);
		p_list[i].y = YConvert(p_list[i].y);
	}

	/*
	 * this only works because p_list is of type Ptype which happens
	 * to be the same as DCpoint. 
	 */
	fill_table = buildFillTable(p_list, (unsigned) n);


	/*
	 * fill with horizontal lines between every other point
	 */
	for (
		yval = fill_table->y_first; 
		yval < (fill_table->y_last + 1); 
		yval+=skip) {

		for (i=0; i<(fill_table->x_count[XC_INDEX(yval)] - 1); i+=2) {

			dev->devline(
				fill_table->x_coord[XC_INDEX(yval)][i], 
				yval,
				fill_table->x_coord[XC_INDEX(yval)][i+1], 
				yval
			);
		}
	}
}




int	PolyLine(c)
CGMC *c;
{
	long	x1,y1,x2,y2;	/* the clipped line coords */
	register long	n,p;
	int	status = 0;

	/*
	 *	Make sure the line attributes are set
	 */
	if (COLOUR_TABLE_DAMAGE) {
		if (dev->update_color_table() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (LINE_COLOUR_DAMAGE) {
		dev->setlinecolour(LINE_COLOUR.index);
		LINE_COLOUR_DAMAGE = FALSE;
	}

	if (LINE_WIDTH_DAMAGE) {
		dev->setlinewidth(LINE_WIDTH);
		LINE_WIDTH_DAMAGE = FALSE;
	}

	if (CLIP_DAMAGE) {
		CoordRect	device_win_coord;
		GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, 
			PackCoordRect(XMIN, YMIN, XMAX,YMAX), CLIPFLAG
		);
		CLIP_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;


	/*
	 * if no clipping use the quick algorithm
	 */

	p = 0;
	while (1) {
		n = 0;

		/* Draw lines in groups of (coordBufSize), except for the
		 * last group. n = count of processed points .
		 * p = count of processed unsent point specifications.
		 */
		while (n < (c->Pnum - 1)) {
			if (Clipper(c->p[n].x,c->p[n].y,c->p[n+1].x,c->p[n+1].y,
				&x1, &y1, &x2, &y2)) {

		
				coordBuf[p].x = x1;
				coordBuf[p].y = y1;
				p++;

				if (p == coordBufSize || p == maxPolyPoints){
					coordBufNum = p;
					dev->point_flush(coordBuf, 
						&coordBufNum, FALSE,FALSE);

					coordBuf[0] =  coordBuf[p-1];
					p = 1;
				}
				if (c->p[n+1].x != x2 || c->p[n+1].y != y2) {
					coordBuf[p].x = x2;
					coordBuf[p].y = y2;

					coordBufNum = p+1;
					dev->point_flush(coordBuf, 
						&coordBufNum, FALSE,FALSE);

					p = 0;
				}
			}
			n++;
		}
		/*
		* see if more flag is set. If so get more data
		*/
		if (c->more) {
			if (Instr_Dec(c) < 1) {
				return (-1);
			}

			if (Clipper(x2,y2,c->p[0].x,c->p[0].y,
				&x1, &y1, &x2, &y2)) {

				coordBuf[p].x = x1;
				coordBuf[p].y = y1;
				p++;
			}
		}
		else break;     /* leave loop   */
	}

	coordBuf[p].x = x2;
	coordBuf[p].y = y2;
	p++;

	if (LINE_WIDTH == 0.0) 
		return(0);	/* do nothing if line is zero width	*/

	if (p > 1) {
		coordBufNum = p;
		dev->point_flush(coordBuf, &coordBufNum, FALSE, FALSE);
	}

	return (status);
}

/*ARGSUSED*/
int  DisjtLine(c)
CGMC *c;
{
	register long	i;
	int	status = 0;

	/*
	 *	Make sure the line attributes are set
	 */
	if (COLOUR_TABLE_DAMAGE) {
		if (dev->update_color_table() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (LINE_COLOUR_DAMAGE) {
		dev->setlinecolour(LINE_COLOUR.index);
		LINE_COLOUR_DAMAGE = FALSE;
	}

	if (LINE_WIDTH_DAMAGE) {
		dev->setlinewidth(LINE_WIDTH);
		LINE_WIDTH_DAMAGE = FALSE;
	}

	if (CLIP_DAMAGE) {
		CoordRect	device_win_coord;
		GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, 
			PackCoordRect(XMIN, YMIN, XMAX,YMAX), CLIPFLAG
		);
		CLIP_DAMAGE = FALSE;
	}


	if (LINE_WIDTH == 0.0) 
		return(0);	/* do nothing if line is zero width	*/

	startedDrawing = TRUE;

	/*
	 * draw line segments one at a time
	 */
	if (ODD(c->Pnum)) c->Pnum--;    /* must be even */
	for(i=0; i<c->Pnum; i+=2) {
		dev->line(c->p[i].x, c->p[i].y, c->p[i+1].x, c->p[i+1].y);
	}
	return(status);
}


int	_PolyMarker(c, fat_dot)
	CGMC *c;
	int	fat_dot;
{
	int	offset;
	int	i;
	int	status = 0;

	CItype	line_colour;
	float	line_width;
	int	line_type;

	VDCtype x,y;
	VDCtype xlen, ylen;

	if (COLOUR_TABLE_DAMAGE) {
		if (dev->update_color_table() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	line_colour = LINE_COLOUR.index;
	line_width = LINE_WIDTH;
	line_type = LINE_TYPE;

	dev->setlinecolour(MARKER_COLOUR.index);
	dev->setlinewidth(1.0);
	dev->setlinestyle(1);

	if (CLIP_DAMAGE) {
		CoordRect	device_win_coord;
		GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, 
			PackCoordRect(XMIN, YMIN, XMAX,YMAX), CLIPFLAG
		);
		CLIP_DAMAGE = FALSE;
	}

	offset = MARKER_SIZE / 2;

	switch (MARKER_TYPE) {
	case MARKER_X:	
		for(i=0;i<c->Pnum;i++) {
			dev->line( c->p[i].x - offset, c->p[i].y - offset,
			      c->p[i].x + offset, c->p[i].y + offset);

			dev->line( c->p[i].x - offset, c->p[i].y + offset,
			      c->p[i].x + offset, c->p[i].y - offset);
		}
		break;

	case MARKER_CIRCLE:
		for(i=0;i<c->Pnum;i++) {
			quick_circle((int) c->p[i].x, (int) c->p[i].y, offset);	
		}
		break;

	case MARKER_STAR:
		for(i=0;i<c->Pnum;i++) {
			dev->line( c->p[i].x - offset, c->p[i].y,
			      c->p[i].x + offset, c->p[i].y);

			dev->line( c->p[i].x, c->p[i].y + offset,
			      c->p[i].x, c->p[i].y - offset);

			dev->line( c->p[i].x - offset, c->p[i].y - offset,
			      c->p[i].x + offset, c->p[i].y + offset);

			dev->line( c->p[i].x - offset, c->p[i].y + offset,
			      c->p[i].x + offset, c->p[i].y - offset);
		}
		break;
	case MARKER_PLUS:
		for(i=0;i<c->Pnum;i++) {
			dev->line( c->p[i].x - offset, c->p[i].y,
			      c->p[i].x + offset, c->p[i].y);

			dev->line( c->p[i].x, c->p[i].y + offset,
			      c->p[i].x, c->p[i].y - offset);
		}
		break;
	case MARKER_DOT:
		for(i=0;i<c->Pnum;i++) {
			/*
			 * see if device needs a larger then normal dot
			 */
			if (!fat_dot && dt->markersize == 1.0) 
				dev->line(c->p[i].x,c->p[i].y,c->p[i].x,c->p[i].y);

			else 
			{	/* fat dot - draw a box not a dot	*/

				/*
				 * the next two lines of code should be 
				 * replaced with info about the device which
				 * tell us how big a box to draw
				 */
				xlen = ABS(XMAX - XMIN) /1000;
				ylen = ABS(YMAX - YMIN) /1000;

				x = c->p[i].x - xlen;
				y = c->p[i].y - ylen;

				dev->line(x,y,x, y+ylen);
				dev->line(x,y+ylen, x+xlen, y+ylen);
				dev->line(x+xlen, y+ylen, x+xlen, y);
				dev->line(x+xlen, y, x, y);
			}
		}
				
		break;
	default:
		ESprintf(
			EINVAL, "Illegal or unsupported marker type(%d)",
			MARKER_TYPE
		);
		return (-1);

	}

	dev->setlinecolour(line_colour);
	dev->setlinewidth(line_width);
	dev->setlinestyle(line_type);

	return (status);
}



/*ARGSUSED*/
int	Polygon(c)
CGMC *c;
{
	int	i;		/* loop variable */
	long	x1,y1,x2,y2;	/* the clipped line coords */
	int	status = 0;

	boolean	draw = FALSE;

        long    num_points = 0; /* number of points to process          */

	extern	long	clipxmax, clipxmin, clipymax, clipymin;

	if (COLOUR_TABLE_DAMAGE) {
		if (dev->update_color_table() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (CLIP_DAMAGE) {	/* has clipping changed	*/
		CoordRect	device_win_coord;
		GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, 
			PackCoordRect(XMIN, YMIN, XMAX,YMAX), CLIPFLAG
		);
		CLIP_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
	 * get the points from the cgmc
	 */


	coordBufNum = 0;
	while (1) {
		num_points += c->Pnum;
		/*	
		 *	make sure point buffer is big enough since we 
		 *	cannot easily break up the polygon like we can a 
		 *	polyline. We also take possible addition bytes
		 *	required for clipping into account.
		 */
		if (coordBufSize < (num_points * 2)) {

			if (! (coordBuf = (Ptype *) 
				realloc ((char *) coordBuf, (unsigned) 
				(num_points*sizeof(Ptype)*2)))) {

				ESprintf(
					errno, "realloc(%d)", 
					num_points * sizeof(Ptype) * 2
				);

				return (-1);
			}

			coordBufSize = num_points * 2;
		}

		/*
		 *	load coordinate buffer with points. Do clipping
		 *	if needed.
		 */
		/*
		 * clip the first edge if necessary.
		 * if the edge is completely outside the clip
		 * rectangle lay it on the boundry
		 */
		if (! Clipper(c->p[0].x, c->p[0].y,
			c->p[1].x,c->p[1].y,
			&x1,&y1,&x2,&y2)) {

			/*
			 * the edge is completely outside the
			 * clip rectangle. bring it back
			 */
			x1 = MIN(clipxmax, c->p[0].x);
			x1 = MAX(clipxmin, x1);
			x2 = MIN(clipxmax, c->p[1].x);
			x2 = MAX(clipxmin, x2);

			y1 = MIN(clipymax, c->p[0].y);
			y1 = MAX(clipymin, y1);
			y2 = MIN(clipymax, c->p[1].y);
			y2 = MAX(clipymin, y2);
		}
		else draw = TRUE;
		coordBuf[coordBufNum].x = x1;
		coordBuf[coordBufNum++].y = y1;

		if (x2 != x1 || y2 != y1){
	
			coordBuf[coordBufNum].x = x2;
			coordBuf[coordBufNum++].y = y2;
		}


		for(i = 2; i < c->Pnum ; i++) {
			if (! Clipper(c->p[i-1].x,c->p[i-1].y,
				c->p[i].x,c->p[i].y,
				&x1,&y1,&x2,&y2)) {

				x1 = MIN(clipxmax, c->p[i - 1].x);
				x1 = MAX(clipxmin, x1);
				x2 = MIN(clipxmax, c->p[i].x);
				x2 = MAX(clipxmin, x2);

				y1 = MIN(clipymax, c->p[i - 1].y);
				y1 = MAX(clipymin, y1);
				y2 = MIN(clipymax, c->p[i].y);
				y2 = MAX(clipymin, y2);
			}
			else draw = TRUE;
			/*
			 * only send the first point if is not the
			 * same as the previous point sent
			 */
			if (x1 != coordBuf[coordBufNum - 1].x ||
				y1 != coordBuf[coordBufNum - 1].y){
	
				coordBuf[coordBufNum].x = x1;
				coordBuf[coordBufNum++].y = y1;
			}
			/*
			 * only send the second point if it is not the
			 * same as the first point
			 */
			if (x2 != x1 || y2 != y1){
	
				coordBuf[coordBufNum].x = x2;
				coordBuf[coordBufNum++].y = y2;
			}
		
		}	/* for	*/

		if (coordBufNum < 3 || !draw) return(0);

		/*
		 * see if more flag is set. If so get more data
		 */
		if (c->more) {
			if (Instr_Dec(c) < 1) {
				return (-1);
			}
		}
		else break;	/* leave loop	*/
	}

	/*
	 * make sure polygon is closed
	 */
	if (coordBuf[0].x != coordBuf[coordBufNum - 1].x ||
		coordBuf[0].y != coordBuf[coordBufNum - 1].y) {

		coordBuf[coordBufNum] = coordBuf[0];
		coordBufNum++;

	} 

	switch(INT_STYLE) {
	case	HOLLOW_S:
		/*
		 * draw hollow polygon with lines => tweek line color
		 */
		dev->setlinecolour(FILL_COLOUR.index);
		LINE_COLOUR_DAMAGE = TRUE;	/* we tweeked the line color */
		dev->point_flush(coordBuf, &coordBufNum, FALSE,FALSE);

		break;
	case	SOLID_S:

		/*
		 * if device has no hardware support for polgons or device has 
		 * poly hardware but can't handle that many points and user 
		 * wants simulation done in this case  => simulate polygon
		 * Or if -softfill option given on command line
		 */
		if (*softFill || coordBufNum > maxPolyPoints) {
			ComSimPoly(
				coordBuf,(int) coordBufNum, commFillScaleFactor
			);
			coordBufNum = 0;
		}
		else {
				
			if (FILL_COLOUR_DAMAGE) {
				dev->setfillcolour(FILL_COLOUR.index);
				FILL_COLOUR_DAMAGE = FALSE;
			}
			dev->point_flush(coordBuf, &coordBufNum, TRUE,FALSE);

		}

		break;
	case	PATTERN_S:
		/*
		 *      code to invoke a pattern routine
		 */
		break;

	case	HATCH_S:
		ComSimHatch(
			coordBuf, coordBufNum, HATCH_IND,
			commHatchScaleFactor, dev
		);
		break;

	case    EMPTY_S:

		/*
		 *      do nothing
		 */
		break;

	default:
		ESprintf(
			EINVAL, "Illegal or unsupported fill style(%d)",
			INT_STYLE
		);
		return(-1);
	}

	return (status);
}





/*	ComLineSim:
 *		This function generates the set of points necessary to 
 *	implement a particular line type, as described by the CGM command
 *	"Line Type", for a **single** line.
 *
 *	on entry: 
 *		x1,y1 : coordinates for one end of the line
 *		x2,y2 : coordinates for other end of the line
 *		LINE_TYPE : is set in the default table to a designated 
 *			line type 2 through 5.
 *		XMAX, XMIN : are set in default table.
 *	on exit:
 *		line_type prints the set pairs of points implementing the type
 *
 *	Note:	If the line type is normal (LINE_TYPE = 1) this routine should
 *		NOT be invoked.
 */

#define	DASH	'-'
#define	DOT	'.'
#define	SPACE	' '

int	ComLineSim (x1,y1,x2,y2)
	VDCtype	x1,y1,x2,y2;
{

	float	delta_x, delta_y;	/* increments along coordinate space
					 * needed to make dots and dashes
					 */
	float	len,			/* length of entire line */
		seglen;			/* length of dot or dash segment */
	float	x,y;
	float	partial;
	const	char	*pptr;		/* pattern pointer	*/
	const	char	*pattern;	/* dash/dot pattern	*/

	static	const char	dash_pattern[] = {
		DASH, SPACE, '\0'
	};
	static	const char	dot_pattern[] = {
		DOT, SPACE, '\0'
	};
	static	const char	dash_dot_pattern[] = {
		DASH, DOT, SPACE, '\0'
	};
	static	const char	dash_dot_dot_pattern[] = {
		DASH, DOT, DOT, SPACE, '\0'
	};
 
	seglen = (XMAX - XMIN) * 0.01;	/*seglen is 0.01*VDC extent	*/

	/*
	 * want to draw in positive x direction to simplify case statements
	 */
	if(x2 < x1) { 	
		VDCtype xtemp, ytemp;

		xtemp = x1; x1 = x2; x2 = xtemp;
		ytemp = y1; y1 = y2; y2 = ytemp;
	}

	/* calculate length of line	*/
	len = sqrt((double)(((x2-x1)*(x2-x1)) + ((y2-y1)*(y2-y1))));

	/*
	 * make sure line length is reasonable.
	 */
	len = ((len < (0.01 * seglen)) ? (0.01 * seglen) : len);

	/*
	 * hack to *attempt* to guarantee that a dashed, and not a solid, 
	 * line is drawn if the lenth of the line is less than $seglen, 
	 * but not too much less.
	 */
	if ((len >= (0.1 * seglen)) && ((len * 0.75) <= seglen)) {
		seglen = 0.75 * len;
	}

	/*calculate x and y coordinates for dot or dash		*/
	delta_x = seglen*(x2-x1)/len;
	delta_y = seglen*(y2-y1)/len;


	switch (LINE_TYPE) {
	case	L_DASH:
		pattern =  dash_pattern;
		break;

	case	L_DOT:
		pattern =  dot_pattern;
		break;

	case	L_DASH_DOT:
		pattern =  dash_dot_pattern;
		break;

	case	L_DASH_DOT_DOT:
		pattern =  dash_dot_dot_pattern;
		break;

	default:
		ESprintf(EINVAL, "Illegal line type(%d)", LINE_TYPE);
		return(-1);

	}

	/*
	 * step through the line in $segment size increments. Draw
	 * whatever is appropriate for each segment, e.g. dash, dot
	 * or space
	 */
	for(partial=0.0, x=x1, y=y1, pptr=pattern; 
		(partial+seglen)<=len; 
		partial+=seglen, x+=delta_x, y+=delta_y, pptr++) {

		if (! *pptr) pptr = pattern;	/* reset pattern	*/

		switch (*pptr) {
		case	DASH:
			dev->line(
				(VDCtype) x, (VDCtype) y, 
				(VDCtype) (x + delta_x), (VDCtype) (y + delta_y)
			);
			break;

		case	DOT:
			dev->line(
				(VDCtype) (x + delta_x),(VDCtype) (y + delta_y),
				(VDCtype) (x + delta_x),(VDCtype) (y + delta_y)
			);
			break;

		case	SPACE:
			break;
		}
		
	}

	if (! *pptr) pptr = pattern;	/* reset pattern	*/

	/*
	 * draw the final *partial* pattern 
	 */
	if (*pptr == DASH) {
		dev->line((VDCtype) x, (VDCtype) y, x2, y2);
	}
	else if (*pptr == DOT) {
		dev->line(x2, y2, x2, y2);
	}
		
	return (0);
}

/*
 *	ComFatLine
 *
 *	Draw a wide line. No attempt is made to join the lines nicely yet
 *
 * on entry
 *	line_width	: number of $pix_width wide lines to draw.
 *	pix_width	: width of a single pixel line in device coords.
 */
#define	DISTANCE(X1, Y1, X2, Y2)	(sqrt ((double) \
	((((X1) - (X2)) * ((X1) - (X2))) + (((Y1) - (Y2)) * ((Y1) - (Y2))))))
	
ComFatLine(x1, y1, x2, y2, line_width, pix_width)
	VDCtype	x1, y1, x2, y2;
	int	line_width;
	int	pix_width;
{
	float	slope;

	VDCtype	dx, dy;		/* increment in x or y direction	*/

	if (line_width == 0) return(0);

	pix_width = (pix_width == 0) ? 1 : pix_width;

	/*
	 * find the slope of the line. We really on care if its greater or
	 * less then 1
	 */
	if (x1 == x2) {
		slope = 2.0;	/* greater then one	*/
	}
	else {
		slope = ((float) (y2 - y1) / (float) (x2 - x1));
	}

	/*
	 * if abs(slope) is less then one we increment the staring point
	 * of each single pixel line in the fat line along the y direction
	 * else in the x direction
	 */
	if (ABS(slope) < 1.0) {
		dy = YScale_(pix_width);
		dx = 0;
	}
	else {
		dx = XScale_(pix_width);
		dy = 0;
		if (x1 != x2 && slope > 0) dx = -dx;
	}

	fat_segment(x1, y1, x2, y2, dx, dy, line_width);

	return(0);
}

