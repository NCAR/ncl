/*
 *	$Id: gcapdev.c,v 1.2 1991-03-12 18:46:02 clyne Exp $
 */
#include <stdio.h>
#include "graphcap.h"
#include "cgmc.h"
#include "soft_fill.h"
#include "translate.h"
#include "ctrandef.h"
#include "default.h"


/*
 * defines for the markers in the strings
 */

#define	VC 	-2	/* Vector Count */
#define	XC 	-3	/* X Coord */
#define	YC 	-4	/* Y Coord */
#define	XYC 	-5	/* X-Y Coord Pair */

gcap_open(max_poly_points)
	unsigned long	*max_poly_points;
{
	*max_poly_points = POLY_MAX_POINTS;

}

gcap_close() 
{
}

/*
 *	Flushes the points in the coord buffer
 *
 *	polyflag  -- if TRUE then used filled polygons if available
 *		  -- if FALSE then use polylines or just lines.
 *	
 *	polysim   -- if TRUE then override the filled polygon and
 *		     simulate the polygon with an outline.
 */
void	gcap_pointflush(coord_buf, coord_buf_num, polyflag,polysim)
Ptype	*coord_buf;
long	*coord_buf_num;
boolean	polyflag;	/* True if flushing a polygon not polylines */
boolean polysim;	/* True if to simulate polygons with lines */
{
	int	currentpoint;
	int	i;
	boolean mass = FALSE;	/* true if can buffer all points at once */

	/*
	 *	If the line style is not the default then send the 
	 *	line segment to the software simulation routine
	 *	to generate the appropriate pattern.
	 */
	if (!polyflag && LINE_TYPE != 1) {
		for(i=1;i<*coord_buf_num;i++)
			(void) ComLineSim(coord_buf[i-1].x,coord_buf[i-1].y,
				  coord_buf[i].x,coord_buf[i].y);
		return;
	}

	/*
	 * 	if line width is not the default (1) and device does not have
	 *	wide line capability simulate wide lines with software
	 */
	if (!polyflag && LINE_WIDTH != 1 && LINE_WIDTH_START_SIZE == 0) {
		for(i=1;i<*coord_buf_num;i++)
			(void) ComFatLine(coord_buf[i-1].x,coord_buf[i-1].y,
					coord_buf[i].x,coord_buf[i].y,
					(int) LINE_WIDTH);
			return;
	}

	/*
	 *	if we are drawing polygons and we have polygon on device	
	 */
	if (polyflag && (POLYGON_START_SIZE > 0) && !polysim) {

		mass = TRUE;
		for(i=0;i<POLYGON_START_SIZE;i++) 
			switch (POLYGON_START[i]) {
			case (char)VC:
				(void)formatveccnt(*coord_buf_num + 1);
				break;
			case (char)XC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    (long)0,
					    1);
				break;
			case (char)YC:
				(void)formatcoord(YConvert(coord_buf[0].y),
					    (long)0,
					    1);
				break;
			case (char)XYC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    YConvert(coord_buf[0].y),
					    2);
				break;
			default:
				buffer(&POLYGON_START[i],1);
				break;
			}

	}

	/*
	 *	if device has polylines
	 *	and we are drawing polylines or simulated polygons 
	 */
	if (POLY_FLAG && (!polyflag || polysim)) {
		mass = TRUE;

		for(i=0;i<LINE_DRAW_START_SIZE;i++) 
			switch (LINE_DRAW_START[i]) {
			case (char)VC:
				if (polyflag)
					(void)formatveccnt(*coord_buf_num + 1);
				else
					(void)formatveccnt(*coord_buf_num);
				break;
			case (char)XC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    (long)0,
					    1);
				break;
			case (char)YC:
				(void)formatcoord(YConvert(coord_buf[0].y),
					    (long)0,
					    1);
				break;
			case (char)XYC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    YConvert(coord_buf[0].y),
					    2);
				break;
			default:
				buffer(&LINE_DRAW_START[i],1);
				break;
			}
	}

	/*
	 *	if we are doing polylines and device has polylines
	 *	or we are doing polygons and device has polygons
	 */
	if (mass) {

		for (currentpoint=0;currentpoint<*coord_buf_num;currentpoint++) {

			
			/*
			 * send line_point_start/polygon_point_start if it exist
			 */
			if (polyflag && !polysim) {
				if (POLY_POINT_START_SIZE)
					buffer(POLY_POINT_START,
							POLY_POINT_START_SIZE); 
			}
			else {
				if (LINE_POINT_START_SIZE)
					buffer(LINE_POINT_START,
							LINE_POINT_START_SIZE);
			}

			/* 
			 * format coordinates pairs
			 */
			(void)formatcoord(XConvert(coord_buf[currentpoint].x),
			    YConvert(coord_buf[currentpoint].y),
			    2);

			if (polyflag && !polysim) {
				if (POLY_POINT_TERM_SIZE)
					buffer(POLY_POINT_TERM,
							POLY_POINT_TERM_SIZE); 
			}
			else {
				if (LINE_POINT_TERM_SIZE)
					buffer(LINE_POINT_TERM,
							LINE_POINT_TERM_SIZE);
			}
		}

		/*
		 * send first point again if polyflag
		 */
		if (polyflag) {

			if (POLY_POINT_START_SIZE)
				buffer(POLY_POINT_START,POLY_POINT_START_SIZE); 

			(void)formatcoord(XConvert(coord_buf[0].x),
					YConvert(coord_buf[0].y), 2);

			if (POLY_POINT_TERM_SIZE)
				buffer(POLY_POINT_TERM,POLY_POINT_TERM_SIZE); 

		}

		if (polyflag && !polysim) {
			if (POLYGON_TERM_SIZE)
				buffer(POLYGON_TERM,POLYGON_TERM_SIZE); 
		}
		else {
			if (LINE_DRAW_TERM_SIZE)
				buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);
		}


		/*
		 *	Set the number of Coords in the Coord buffer to zero
		 */
		*coord_buf_num = 0;
		return;

	}


	/*
	 *	The device does not have a polyline (or polygon instruction
	 *	as the case may be) so then do each line one at a time.
 	 */
	if (LINE_MOVE_START_SIZE)
		buffer(LINE_MOVE_START,LINE_MOVE_START_SIZE);

	(void)formatcoord(XConvert(coord_buf[0].x),
		    YConvert(coord_buf[0].y),
		    2);

	if (LINE_MOVE_TERM_SIZE)
		buffer(LINE_MOVE_TERM,LINE_MOVE_TERM_SIZE);

	for (currentpoint=1;currentpoint < *coord_buf_num;currentpoint++){

		if (LINE_DRAW_START_SIZE)
			buffer(LINE_DRAW_START,LINE_DRAW_START_SIZE);

		(void)formatcoord(XConvert(coord_buf[currentpoint].x),
			    YConvert(coord_buf[currentpoint].y), 2);

		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);

	}

	if (polyflag) {

		if (LINE_DRAW_START_SIZE)
			buffer(LINE_DRAW_START,LINE_DRAW_START_SIZE);

		(void)formatcoord(XConvert(coord_buf[0].x),
			    YConvert(coord_buf[0].y), 2); 

		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);
	}

	if (polyflag && (POLYGON_START_SIZE > 0) && !polysim)
		buffer(POLYGON_TERM,POLYGON_TERM_SIZE); 

	/*
 	 *	Set the number of Coords in the Coord buffer to zero
	 */
	*coord_buf_num = 0;
}

/* 
 * 	Take two coords and generals the command to draw a line between them
 */
void	gcap_line(x1_,y1_,x2_,y2_)
long	x1_,y1_,x2_,y2_;
{
	int	i;	/* loop variable */

	long	x1, y1, x2, y2;
	extern	short	devWinSet;

	if (CLIPFLAG || devWinSet) {
		if (!(Clipper(x1_, y1_, x2_, y2_, &x1, &y1, &x2, &y2))) {
			return;
		}
	} else {
		x1 = x1_; y1 = y1_; x2 = x2_; y2 = y2_;
	}

	if (POLY_FLAG) {
		for(i=0;i<LINE_DRAW_START_SIZE;i++) 
			switch (LINE_DRAW_START[i]) {
			case (char)VC:
				(void)formatveccnt((long)2);
				break;
			case (char)XC:
				(void)formatcoord(XConvert(x1),
					    (long)0,
					    1);
				break;
			case (char)YC:
				(void)formatcoord(YConvert(y1),
					    (long)0,
					    1);
				break;
			case (char)XYC:
				(void)formatcoord(XConvert(x1), YConvert(y1), 2);
				break;
			default:
				buffer(&LINE_DRAW_START[i],1);
				break;
			}


		/*
		 * send the coordinates of the line
		 */
		if (LINE_POINT_START_SIZE)
			buffer(LINE_POINT_START,LINE_POINT_START_SIZE); 

		(void)formatcoord(XConvert(x1), YConvert(y1), 2);
		if (LINE_POINT_TERM_SIZE)
			buffer(LINE_POINT_TERM,LINE_POINT_TERM_SIZE);

		if (LINE_POINT_START_SIZE)
			buffer(LINE_POINT_START,LINE_POINT_START_SIZE); 

		(void)formatcoord(XConvert(x2), YConvert(y2), 2);
		if (LINE_POINT_TERM_SIZE)
			buffer(LINE_POINT_TERM,LINE_POINT_TERM_SIZE);



		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);

	} else {
	
		if (LINE_MOVE_START_SIZE)
			buffer(LINE_MOVE_START,LINE_MOVE_START_SIZE);

		(void)formatcoord(XConvert(x1), YConvert(y1), 2);
		if (LINE_MOVE_TERM_SIZE)
			buffer(LINE_MOVE_TERM,LINE_MOVE_TERM_SIZE);

		if (LINE_DRAW_START_SIZE)
			buffer(LINE_DRAW_START,LINE_DRAW_START_SIZE);

		(void)formatcoord(XConvert(x2), YConvert(y2), 2);
		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);
	}
}

/*
 *	dev_line: same as line() except don't convert coordinates from
 *	VDC to DC. This has already been done
 */
void	gcap_devline(x1, y1, x2, y2)
	DCtype	x1, y1, x2, y2;
{
	int	i;	/* loop variable */

	if (POLY_FLAG) {
		for(i=0;i<LINE_DRAW_START_SIZE;i++) 
			switch (LINE_DRAW_START[i]) {
			case (char)VC:
				(void)formatveccnt((long)2);
				break;
			case (char)XC:
				(void)formatcoord((long) x1, (long)0, 1);
				break;
			case (char)YC:
				(void)formatcoord((long) y1, (long)0, 1);
				break;
			case (char)XYC:
				(void)formatcoord((long) x1, (long) y1, 2);
				break;
			default:
				buffer(&LINE_DRAW_START[i],1);
				break;
			}


		/*
		 * send the coordinates of the line
		 */
		if (LINE_POINT_START_SIZE)
			buffer(LINE_POINT_START,LINE_POINT_START_SIZE); 

		(void)formatcoord((long) x1, (long) y1, 2);
		if (LINE_POINT_TERM_SIZE)
			buffer(LINE_POINT_TERM,LINE_POINT_TERM_SIZE);

		if (LINE_POINT_START_SIZE)
			buffer(LINE_POINT_START,LINE_POINT_START_SIZE); 

		(void)formatcoord((long) x2, (long) y2, 2);
		if (LINE_POINT_TERM_SIZE)
			buffer(LINE_POINT_TERM,LINE_POINT_TERM_SIZE);



		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);

	} else {
	
		if (LINE_MOVE_START_SIZE)
			buffer(LINE_MOVE_START,LINE_MOVE_START_SIZE);

		(void)formatcoord((long) x1, (long) y1, 2);
		if (LINE_MOVE_TERM_SIZE)
			buffer(LINE_MOVE_TERM,LINE_MOVE_TERM_SIZE);

		if (LINE_DRAW_START_SIZE)
			buffer(LINE_DRAW_START,LINE_DRAW_START_SIZE);

		(void)formatcoord((long) x2, (long) y2, 2);
		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);
	}
}



/*
 *	Takes a line style and changes the style in the default table
 *	This is all that is necessary since the we generate the line styles
 *	in software.
 */ 
void	gcap_linestyle(style)
int	style;
{
	LINE_TYPE = style;
}
/*
 *	The following three functions are used with in this module
 *	to change the given line attributes.
 */

/*
 *	Takes a colour index and generates the commands to change the 
 *	line colour.
 */ 
void	gcap_linecolour(index)
CItype	index;
{
	/*
	 * some devices require special color setting instruction if we are		 * drawing in the background colour which is designated by index 0
	 */
	if (index == 0 && LINE_BACK_COL_START_SIZE) {
		buffer(LINE_BACK_COL_START,LINE_BACK_COL_START_SIZE);
	}
	else
		buffer(LINE_COLOUR_START,LINE_COLOUR_START_SIZE);

	(void)formatindex(index,FALSE);

	if (index == 0 && LINE_BACK_COL_TERM_SIZE) {
		buffer(LINE_BACK_COL_TERM,LINE_BACK_COL_TERM_SIZE);
	}
	else
		buffer(LINE_COLOUR_TERM,LINE_COLOUR_TERM_SIZE);

	/*
	 * This is a hack since PostScript
	 * Poly_Colour command is the same
	 * as the Line_Colour.
	 */
	FILL_COLOUR_DAMAGE = TRUE;

}

	


void	gcap_fillcolour(index)
CItype	index;
{

	/*
	 * some devices require special color setting instruction if we are		 * drawing in the background colour which is designated by index 0
	 */
	if (index == 0 && POLY_BACK_COL_START_SIZE) {
		buffer(POLY_BACK_COL_START, POLY_BACK_COL_START_SIZE);
	}
	else
		buffer(POLYGON_COLOUR_START, POLYGON_COLOUR_START_SIZE);

	(void)formatindex(index,TRUE);

	if (index == 0 && POLY_BACK_COL_TERM_SIZE) {
		buffer(POLY_BACK_COL_TERM, POLY_BACK_COL_TERM_SIZE);
	}
	else
		buffer(POLYGON_COLOUR_TERM, POLYGON_COLOUR_TERM_SIZE);

	/*
	 * This is a hack since PostScript
	 * Poly_Colour command is the same
	 * as the Line_Colour.
	 */
	LINE_COLOUR_DAMAGE = TRUE;

}

/*
 *	Takes a line width and generates the commands to change the 
 *	line colour.
 */ 
void	gcap_linewidth(width)
float	width;
{

	if (LINE_WIDTH_START_SIZE == 0)
		return;

	buffer(LINE_WIDTH_START, LINE_WIDTH_START_SIZE);

	width *= 8.0 * LINE_WIDTH_SCALE;

	if ((int)width < LINE_WIDTH_MIN)
		(void)formatwidth(LINE_WIDTH_MIN);
	else if ((int)width > LINE_WIDTH_MAX)
		(void)formatwidth(LINE_WIDTH_MAX);
	else
		(void)formatwidth((int)width);

	buffer(LINE_WIDTH_TERM, LINE_WIDTH_TERM_SIZE);

}
