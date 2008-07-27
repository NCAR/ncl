/*
 *	$Id: gcapdev.c,v 1.29 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1995                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/c.h>
#include "ctrans.h"
#include "graphcap.h"
#include "cgmc.h"
#include "soft_fill.h"
#include "translate.h"
#include "ctrandef.h"
#include "default.h"
#include "format.h"

extern	boolean	doSimulateBG;
extern	boolean	startedDrawing;

/*
 * defines for the markers in the strings
 */

#define	VC 	-2	/* Vector Count */
#define	XC 	-3	/* X Coord */
#define	YC 	-4	/* Y Coord */
#define	XYC 	-5	/* X-Y Coord Pair */



/*
 * simulate background color setting
 */
static	int	simulate_bg_color()
{
	static	boolean first = TRUE;
	static	CGMC	cgmc;
	int	status = 0;


	Etype	fill_style = INT_STYLE;
	Etype	clip_flag = CLIPFLAG;
	CItype	fill_color = FILL_COLOUR.index;

        /*
	 * create space for temp cmgc
	 */
	if (first) {
		if (! (cgmc.e = (Etype *) malloc (sizeof(Etype)))) {
			ESprintf(errno, "malloc(%d)", sizeof(Etype));
			return(-1);
		}
		if (! (cgmc.ci = (CItype *) malloc (sizeof(CItype)))) {
			ESprintf(errno, "malloc(%d)", sizeof(CItype));
			return(-1);
		}
		if (! (cgmc.p = (Ptype *) malloc (5 * sizeof(Ptype)))) {
			ESprintf(errno, "malloc(%d)", 5 * sizeof(Ptype));
			return(-1);
		}
		first = FALSE;
	}

	if (COLOUR_TABLE_DAMAGE) {
		if (gcap_update_color_table() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	cgmc.more = FALSE;

        /* set fill colour to background color	*/
        cgmc.cgmclass = ATT_ELEMENT;
        cgmc.command = FILL_COLOUR_ID;
        cgmc.ci[0] = 0;		/* color index zero is background color	*/
        cgmc.CInum = 1;
        (void) Process(&cgmc);

        /* set interior style to solid	*/
        cgmc.cgmclass = ATT_ELEMENT;
        cgmc.command = INTERIOR_STYLE_ID;
        cgmc.e[0] = SOLID_S;
        cgmc.Enum = 1;
        (void) Process(&cgmc);

        /* turn off clipping	*/
        cgmc.cgmclass = CON_ELEMENT;
        cgmc.command = CLIP_INDICATOR_ID;
        cgmc.e[0] = 0;
        cgmc.Enum = 1;
        (void) Process(&cgmc);


	/*
	 * draw a big rectangle
	 */
        cgmc.cgmclass = GRP_ELEMENT;
        cgmc.command = POLYGON_ID;
	cgmc.p[0].x = XMIN;
	cgmc.p[0].y = YMIN;
	cgmc.p[1].x = XMAX;
	cgmc.p[1].y = YMIN;
	cgmc.p[2].x = XMAX;
	cgmc.p[2].y = YMAX;
	cgmc.p[3].x = XMIN;
	cgmc.p[3].y = YMAX;
	cgmc.Pnum = 4;
        (void) Process(&cgmc);

	/*
	 * restore polygon attributes
	 */
        /* set fill colour to background color	*/
        cgmc.cgmclass = ATT_ELEMENT;
        cgmc.command = FILL_COLOUR_ID;
        cgmc.ci[0] = fill_color;
        cgmc.CInum = 1;
        (void) Process(&cgmc);

        /* set interior style to solid	*/
        cgmc.cgmclass = ATT_ELEMENT;
        cgmc.command = INTERIOR_STYLE_ID;
        cgmc.e[0] = fill_style;
        cgmc.Enum = 1;
        (void) Process(&cgmc);

        /* restore clipping	*/
        cgmc.cgmclass = CON_ELEMENT;
        cgmc.command = CLIP_INDICATOR_ID;
        cgmc.e[0] = clip_flag;
        cgmc.Enum = 1;
        (void) Process(&cgmc);

	return(status);
}

void	gcap_open(max_poly_points)
	unsigned long	*max_poly_points;
{
	*max_poly_points = POLY_MAX_POINTS;

}

void	gcap_close() 
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
	int	currentpoint = 0;
	int	i;
	boolean mass = FALSE;	/* true if can buffer all points at once */
	SignedChar	s_char_;

	int	line_width = ROUND(LINE_WIDTH);

	int	ComLineSim();

	if (line_width == 0 && ! polyflag) return;


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
	if (!polyflag && line_width != 1 && LINE_WIDTH_START_SIZE == 0) {
		for(i=1;i<*coord_buf_num;i++)
			(void) ComFatLine(
				coord_buf[i-1].x,coord_buf[i-1].y,
				coord_buf[i].x,coord_buf[i].y,
				line_width, (int) XScale(POLY_SIM_SPACING)
			);
			return;
	}

	/*
	 *	if we are drawing polygons and we have polygon on device	
	 */
	if (polyflag && (POLYGON_START_SIZE > 0) && !polysim) {

		mass = TRUE;
		for(i=0;i<POLYGON_START_SIZE;i++) {
			s_char_ = (SignedChar) POLYGON_START[i];
			switch ((int) s_char_) {
			case VC:
				(void)formatveccnt(*coord_buf_num + 1);
				break;
			case XC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    (long)0,
					    1);
				break;
			case YC:
				(void)formatcoord(YConvert(coord_buf[0].y),
					    (long)0,
					    1);
				break;
			case XYC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    YConvert(coord_buf[0].y),
					    2);
				break;
			default:
				buffer(&s_char_,1);
				break;
			}
		}

	}

	/*
	 *	if device has polylines
	 *	and we are drawing polylines or simulated polygons 
	 */
	if (POLY_FLAG && (!polyflag || polysim)) {
		mass = TRUE;

		for(i=0;i<LINE_DRAW_START_SIZE;i++) {
			s_char_ = (SignedChar) LINE_DRAW_START[i];
			switch ((int) s_char_) {
			case VC:
				if (polyflag)
					(void)formatveccnt(*coord_buf_num + 1);
				else
					(void)formatveccnt(*coord_buf_num);
				break;
			case XC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    (long)0,
					    1);
				currentpoint = 1;
				break;
			case YC:
				(void)formatcoord(YConvert(coord_buf[0].y),
					    (long)0,
					    1);
				currentpoint = 1;
				break;
			case XYC:
				(void)formatcoord(XConvert(coord_buf[0].x),
					    YConvert(coord_buf[0].y),
					    2);
				currentpoint = 1;
				break;
			default:
				buffer(&s_char_,1);
				break;
			}
		}
	}

	/*
	 *	if we are doing polylines and device has polylines
	 *	or we are doing polygons and device has polygons
	 */
	if (mass) {

		for ( ;currentpoint<*coord_buf_num;currentpoint++) {

			
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
	SignedChar	s_char_;
	boolean		first_point_sent = FALSE;


	if (!(Clipper(x1_, y1_, x2_, y2_, &x1, &y1, &x2, &y2))) {
		return;
	}

	if (POLY_FLAG) {
		for(i=0;i<LINE_DRAW_START_SIZE;i++) {
			s_char_ = (SignedChar) LINE_DRAW_START[i];
			switch ((int) s_char_) {
			case VC:
				(void)formatveccnt((long)2);
				break;
			case XC:
				(void)formatcoord(XConvert(x1),
					    (long)0,
					    1);
				first_point_sent = TRUE;
				break;
			case YC:
				(void)formatcoord(YConvert(y1),
					    (long)0,
					    1);
				first_point_sent = TRUE;
				break;
			case XYC:
				(void)formatcoord(XConvert(x1), YConvert(y1),2);
				first_point_sent = TRUE;
				break;
			default:
				buffer(&s_char_,1);
				break;
			}
		}


		/*
		 * send the coordinates of the line. Only send the first
		 * point if it was not sent as part of the LINE_DRAW_START.
		 */
		if (! first_point_sent) {
			if (LINE_POINT_START_SIZE)
				buffer(LINE_POINT_START,LINE_POINT_START_SIZE); 

			(void)formatcoord(XConvert(x1), YConvert(y1), 2);
			if (LINE_POINT_TERM_SIZE)
				buffer(LINE_POINT_TERM,LINE_POINT_TERM_SIZE);
		}

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
	SignedChar	s_char_;
	boolean		first_point_sent = FALSE;


	if (POLY_FLAG) {
		for(i=0;i<LINE_DRAW_START_SIZE;i++) {
			s_char_ = (SignedChar) LINE_DRAW_START[i];
			switch ((int) s_char_) {
			case VC:
				(void)formatveccnt((long)2);
				break;
			case XC:
				(void)formatcoord((long) x1, (long)0, 1);
				first_point_sent = TRUE;
				break;
			case YC:
				(void)formatcoord((long) y1, (long)0, 1);
				first_point_sent = TRUE;
				break;
			case XYC:
				(void)formatcoord((long) x1, (long) y1, 2);
				first_point_sent = TRUE;
				break;
			default:
				buffer(&s_char_,1);
				break;
			}
		}


		/*
		 * send the coordinates of the line. Only send the first
		 * point if it was not sent as part of the LINE_DRAW_START.
		 */
		if (! first_point_sent) {
			if (LINE_POINT_START_SIZE)
				buffer(LINE_POINT_START,LINE_POINT_START_SIZE); 

			(void)formatcoord((long) x1, (long) y1, 2);
			if (LINE_POINT_TERM_SIZE)
				buffer(LINE_POINT_TERM,LINE_POINT_TERM_SIZE);
		}

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
	int	iwidth;

	if (LINE_WIDTH_START_SIZE == 0)
		return;

	width *= 8.0 * LINE_WIDTH_SCALE;
	width = width < 0.0 ? 0.0 : width;

	iwidth = (int) ROUND(width);

	buffer(LINE_WIDTH_START, LINE_WIDTH_START_SIZE);

	if (iwidth < LINE_WIDTH_MIN)
		(void)formatwidth(LINE_WIDTH_MIN);
	else if (iwidth > LINE_WIDTH_MAX)
		(void)formatwidth(LINE_WIDTH_MAX);
	else
		(void)formatwidth(iwidth);

	buffer(LINE_WIDTH_TERM, LINE_WIDTH_TERM_SIZE);

}


/*
 *
 */
/*
 * defines for the markers in the strings
 */
#define MAD     -6      /* Colour Index */

int	gcap_update_color_table()
{
	int	i,k;		/* loop var */
	long	data[3];
	boolean	skipping,
		defining;
	boolean	do_background = FALSE;
	SignedChar	s_char_;
	int	status = 0;


	if (!COLOUR_AVAIL)
		return(0);
	if (!MAP_AVAIL)
		return(0);

        /*
         * any time we change the colour table we "damage" the colour
         * attributes
         */
        FILL_COLOUR_DAMAGE = TRUE;
        MARKER_COLOUR_DAMAGE = TRUE;
        LINE_COLOUR_DAMAGE = TRUE;

	do_background = (boolean) COLOUR_INDEX_DAMAGE(0);
	/*
	 * don't change background color if we've begun drawing
	 */
	if (COLOUR_INDEX_DAMAGE(0) && startedDrawing) { 
		ESprintf(E_UNKNOWN, "Background color changes ignored after drawing has begun");
		status = -1;

		COLOUR_INDEX_DAMAGE(0) = FALSE;
                COLOUR_TOTAL_DAMAGE--;
		do_background = FALSE;
	}




	if (MAP_INDIVIDUAL) {
	for(i=0; COLOUR_TOTAL_DAMAGE > 0 && i<=MAX_C_I; i++) {

	if (COLOUR_INDEX_DAMAGE(i)) {

		for (k=0;k<MAP_START_SIZE;k++) {
			s_char_ = (SignedChar) MAP_START[k];
			switch ((int) s_char_) {
			case MAD:
				(void)formatindex((long) i,FALSE);
				break;
			default: 
				buffer(&s_char_,1);
				break;
			}
		}

		switch ((int) MAP_MODEL) {
		case 0: /* mono */
			data[0] = (0.3 * COLOUR_INDEX_RED(i)) + 
				(0.6 * COLOUR_INDEX_GREEN(i)) +
				(0.1 * COLOUR_INDEX_BLUE(i));

			(void)formatintensity(data, 1);
			break;
		case 1:	/* RGB	*/
			data[0] = COLOUR_INDEX_RED(i);
			data[1] = COLOUR_INDEX_GREEN(i);
			data[2] = COLOUR_INDEX_BLUE(i);

			(void)formatintensity(data, 3);
			break;
		case 2:	/* BGR	*/
			data[0] = COLOUR_INDEX_RED(i);
			data[1] = COLOUR_INDEX_GREEN(i);
			data[2] = COLOUR_INDEX_BLUE(i);

			(void)formatintensity(data, 3);
			break;
		case  3: /* HLS */
			RGBtoHLS (
				(long) COLOUR_INDEX_RED(i), 
				(long) COLOUR_INDEX_GREEN(i), 
				(long) COLOUR_INDEX_BLUE(i),
				&data[0],
				&data[1],
				&data[2]
			);

			(void)formatintensity(data, 3);
			break;
		default:
			ESprintf(
				E_UNKNOWN, 
				"Invalid viewport format [ %s ]", ErrGetMsg()
			);
			break;
		}

		COLOUR_TOTAL_DAMAGE--;
		COLOUR_INDEX_DAMAGE(i) = FALSE;


		buffer(MAP_TERM,MAP_TERM_SIZE);
	}	/* if	damage */
	}	/* for	*/
	}	/* if individual	*/


	/*
	 * not individual
	 */
	else {


	skipping = TRUE;
	defining = FALSE;
	for(i=0; COLOUR_TOTAL_DAMAGE > 0 && i<=MAX_C_I; i++) {

		if (COLOUR_INDEX_DAMAGE(i)) {
			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;
			if (! defining) {
				/*
				 * begin a new run of colors
				 */

				for (k=0;k<MAP_START_SIZE;k++) {
					s_char_ = (SignedChar) MAP_START[k];
					switch ((int) s_char_) {
					case MAD:
						(void)formatindex((long)i,FALSE);
						break;
					default: 
						buffer(&s_char_,1);
						break;
					}
				}
				defining = TRUE;
				skipping = FALSE;
			}

			switch ((int) MAP_MODEL) {
			case 0: /* mono */
				data[0] = (0.3 * COLOUR_INDEX_RED(i)) + 
					(0.6 * COLOUR_INDEX_GREEN(i)) +
					(0.1 * COLOUR_INDEX_BLUE(i));

			(void)formatintensity(data, 1);
			break;
			case 1:	/* RGB	*/
				data[0] = COLOUR_INDEX_RED(i);
				data[1] = COLOUR_INDEX_GREEN(i);
				data[2] = COLOUR_INDEX_BLUE(i);

				(void)formatintensity(data, 3);
				break;
			case 2:	/* BGR	*/
				data[0] = COLOUR_INDEX_RED(i);
				data[1] = COLOUR_INDEX_GREEN(i);
				data[2] = COLOUR_INDEX_BLUE(i);

				(void)formatintensity(data, 3);
				break;
			case  3: /* HLS */
				RGBtoHLS (
					(long) COLOUR_INDEX_RED(i), 
					(long) COLOUR_INDEX_GREEN(i), 
					(long) COLOUR_INDEX_BLUE(i),
					&data[0],
					&data[1],
					&data[2]
				);

				(void)formatintensity(data, 3);
				break;
			}

			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;


		}	/* if	damage */
		else {
			if (! skipping) {
				buffer(MAP_TERM,MAP_TERM_SIZE);
				skipping = TRUE;
				defining = FALSE;
			}
		}
	}	/* for	*/
	}	/* else if  individual	*/

	if (doSimulateBG && do_background) {
		BACKCOLR_DAMAGE = FALSE;
		(void)	simulate_bg_color();
	}

	return(status);
}
