/*
 *	$Id: gcap.c,v 1.3 1991-01-09 11:10:19 clyne Exp $
 */
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
 *	The module contains the graphic routines for the graph cap processing.
 *
 *	The interface to all of these routines is the CGMC data type.
 *	This is typedef'ed in cgmc.h and is talked about in the maintenance
 *	manual.
 *
 *	You will notice that there may be empty procedures that can later
 *	be filled in to further the move toward this being a full
 *	CGM Translator.
 *
 * rev 1.01 clyne 2/16/90	: fixed bug in binary encoding of colour
 *                                intensities
 * rev 1.02 clyne 2/27/90	: erroneous error message generated when color
 *				  index exceeded initial palette size
 * rev 1.03 clyne 3/07/90	: max device color table size not checked by
 *				  ColrTable();
 * rev 1.04 clyne 3/09/90	: added clipping made necessary by device 
 *				  window specifications
 * rev 1.05 clyne 3/14/90	: polygons clipped incorrectly
 * rev 1.06 clyne 3/28/90	: polylines clipped incorrectly
 * rev 1.07 clyne 3/29/90	: cell arrays drawn as a box if clipping 
 *				  is required.
 * rev 1.08 clyne 4/24/90	: extra 'clear page' sent on non-batch devices
 */

#include <stdio.h>
#include <math.h>

#ifdef	SYSV
#include	<sys/types.h>
#endif

#ifndef	L_SET
#define	L_SET	0
#endif

#include <cterror.h>
#include <ncarv.h>
#include "cgmc.h"
#include "graphcap.h"
#include "default.h"
#include "ctrandef.h"
#include "soft_fill.h"
#include "translate.h"

#ifndef lint
static char *RCSid = "$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libctrans/gcap.c,v 1.3 1991-01-09 11:10:19 clyne Exp $";
#endif

extern	FILE	*tty;
extern	short	devWinSet;
extern	long	lseek();
extern	Ct_err	formatintensity();
extern	Ct_err	formatveccnt();
extern	Ct_err	formatcoord();
extern	Ct_err	formatindex();
extern	Ct_err	formatwidth();
extern	Ct_err	raster();
extern	Ct_err	line_sim();
extern	boolean	Batch;
extern	boolean	deviceIsInit;

static	CoordRect	deviceWinCoord;
static	CoordRect	VDCExtent;

extern	boolean	*softFill;


/*
 *	A buffer holding a lot of x,y pairs so that as the clipping is done
 *	we have a place to put them.  This is necessary because in some cases
 *	it will be necessary to know the number of coord pair before sending
 *	any of the pairs.
 */
static Ptype	*coordBuf = NULL;
static long	coordBufNum = 0;	/* the number of coords 
						currently in the buffer */
static long	coordBufSize = 0;	/* amount mem allocated		*/

#define	ABS(X)          ((X) < 0 ? -(X) : (X))
#define	MIN(A,B)	((A) < (B) ? (A) : (B))
#define	MAX(A,B)	((A) > (B) ? (A) : (B))

static	int	fillScaleFactor;

/*
 * defines for the markers in the strings
 */

#define	VC 	-2	/* Vector Count */
#define	XC 	-3	/* X Coord */
#define	YC 	-4	/* Y Coord */
#define	XYC 	-5	/* X-Y Coord Pair */
#define	MAD	-6	/* Colour Index */


/*
 *	Flushes the points in the coord buffer
 *
 *	polyflag  -- if TRUE then used filled polygons if available
 *		  -- if FALSE then use polylines or just lines.
 *	
 *	polysim   -- if TRUE then override the filled polygon and
 *		     simulate the polygon with an outline.
 */
PointFlush(polyflag,polysim)
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
		for(i=1;i<coordBufNum;i++)
			(void) line_sim(coordBuf[i-1].x,coordBuf[i-1].y,
				  coordBuf[i].x,coordBuf[i].y);

	}


	/*
	 *	if we are drawing polygons and we have polygon on device	
	 */
	if (polyflag && (POLYGON_START_SIZE > 0) && !polysim) {

		mass = TRUE;
		for(i=0;i<POLYGON_START_SIZE;i++) 
			switch (POLYGON_START[i]) {
			case (char)VC:
				(void)formatveccnt(coordBufNum + 1);
				break;
			case (char)XC:
				(void)formatcoord(XConvert(coordBuf[0].x),
					    (long)0,
					    1);
				break;
			case (char)YC:
				(void)formatcoord(YConvert(coordBuf[0].y),
					    (long)0,
					    1);
				break;
			case (char)XYC:
				(void)formatcoord(XConvert(coordBuf[0].x),
					    YConvert(coordBuf[0].y),
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
					(void)formatveccnt(coordBufNum + 1);
				else
					(void)formatveccnt(coordBufNum);
				break;
			case (char)XC:
				(void)formatcoord(XConvert(coordBuf[0].x),
					    (long)0,
					    1);
				break;
			case (char)YC:
				(void)formatcoord(YConvert(coordBuf[0].y),
					    (long)0,
					    1);
				break;
			case (char)XYC:
				(void)formatcoord(XConvert(coordBuf[0].x),
					    YConvert(coordBuf[0].y),
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

		for (currentpoint=0;currentpoint<coordBufNum;currentpoint++) {

			
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
			(void)formatcoord(XConvert(coordBuf[currentpoint].x),
			    YConvert(coordBuf[currentpoint].y),
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

			(void)formatcoord(XConvert(coordBuf[0].x),
					YConvert(coordBuf[0].y), 2);

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
		coordBufNum = 0;
		return;

	}


	/*
	 *	The device does not have a polyline (or polygon instruction
	 *	as the case may be) so then do each line one at a time.
 	 */
	if (LINE_MOVE_START_SIZE)
		buffer(LINE_MOVE_START,LINE_MOVE_START_SIZE);

	(void)formatcoord(XConvert(coordBuf[0].x),
		    YConvert(coordBuf[0].y),
		    2);

	if (LINE_MOVE_TERM_SIZE)
		buffer(LINE_MOVE_TERM,LINE_MOVE_TERM_SIZE);

	for (currentpoint=1;currentpoint < coordBufNum;currentpoint++){

		if (LINE_DRAW_START_SIZE)
			buffer(LINE_DRAW_START,LINE_DRAW_START_SIZE);

		(void)formatcoord(XConvert(coordBuf[currentpoint].x),
			    YConvert(coordBuf[currentpoint].y), 2);

		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);

	}

	if (polyflag) {

		if (LINE_DRAW_START_SIZE)
			buffer(LINE_DRAW_START,LINE_DRAW_START_SIZE);

		(void)formatcoord(XConvert(coordBuf[0].x),
			    YConvert(coordBuf[0].y), 2); 

		if (LINE_DRAW_TERM_SIZE)
			buffer(LINE_DRAW_TERM,LINE_DRAW_TERM_SIZE);
	}

	if (polyflag && (POLYGON_START_SIZE > 0) && !polysim)
		buffer(POLYGON_TERM,POLYGON_TERM_SIZE); 

	/*
 	 *	Set the number of Coords in the Coord buffer to zero
	 */
	coordBufNum = 0;
}

/* 
 * 	Take two coords and generals the command to draw a line between them
 */
line(x1_,y1_,x2_,y2_)
long	x1_,y1_,x2_,y2_;
{
	int	i;	/* loop variable */

	long	x1, y1, x2, y2;

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
dev_line(x1, y1, x2, y2)
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
 *	sim_polygon
 *	use software to simulate a filled polygon
 */
static	sim_polygon(p_list, n)
	Ptype	*p_list;
	int	n;
{
	FillTable       *fill_table;
	int     j;
	DCtype	i;
	extern  FillTable       *buildFillTable();
	Ct_err	setlinecolour();
 

	if (n < 2)
		return;
	/*
	 * check and set color. This is complicated since we are using
	 * lines to fill polygons.
	 */
	if (COLOUR_AVAIL && FILL_COLOUR_DAMAGE) {
		(void) setlinecolour(FILL_COLOUR.index);/* set LINE color */
		LINE_COLOUR_DAMAGE = TRUE;
		FILL_COLOUR_DAMAGE = FALSE;
	}

	/*
	 * convert VDC coordinates to DC (device) coordinates
	 * (note this only legal since VDCtype and DCtype are the same type)
	 */
	for (i = 0; i < n; i++) {
		p_list[i].x = XConvert(p_list[i].x);
		p_list[i].y = YConvert(p_list[i].y) / fillScaleFactor;
	}

	/*
	 * this only works because p_list is of type Ptype which happens
	 * to be the same as DCpoint. 
	 */
	fill_table = buildFillTable(p_list, (unsigned) n);

	/*
	 * fill with horizontal lines between every other point
	 */
	for (i = fill_table->y_first; i < (fill_table->y_last + 1); i++) {
		for (j = 0; j < (fill_table->x_count[i] - 1); j+=2) {

			dev_line( fill_table->x_coord[i][j], 
				i * fillScaleFactor,
				fill_table->x_coord[i][j+1], 
				i * fillScaleFactor);
		}
	}
}

/*
 *	The following three functions are used with in this module
 *	to change the given line attributes.
 */
	

/*
 *	Takes a colour index and generates the commands to change the 
 *	line colour.
 */ 
Ct_err	setlinecolour(index)
CItype	index;
{
	if (!COLOUR_AVAIL) 
		return (OK);

	if (index >= MAP_INDEX_MAX) {
		return(OK);
	}
	/*
	 * some devices require special color setting instruction if we are		 * drawing in the background colour which is designated by index 0
	 */
	if (index == 0 && LINE_BACK_COL_START_SIZE)
		buffer(LINE_BACK_COL_START,LINE_BACK_COL_START_SIZE);
	else
		buffer(LINE_COLOUR_START,LINE_COLOUR_START_SIZE);


	(void)formatindex(index,FALSE);

	if (index == 0 && LINE_BACK_COL_TERM_SIZE) 
		buffer(LINE_BACK_COL_TERM,LINE_BACK_COL_TERM_SIZE);
	else
		buffer(LINE_COLOUR_TERM,LINE_COLOUR_TERM_SIZE);

	/*
	 * This is a hack since PostScript
	 * Poly_Colour command is the same
	 * as the Line_Colour.
	 */
	FILL_COLOUR_DAMAGE = TRUE;

	return(OK);
}

Ct_err	setfillcolour(index)
CItype	index;
{

	if (!COLOUR_AVAIL) 
		return(OK);

	if (index >= MAP_INDEX_MAX) {	/* colour index out of range	*/
		return(OK);
	}
	/*
	 * some devices require special color setting instruction if we are		 * drawing in the background colour which is designated by index 0
	 */
	if (index == 0 && POLY_BACK_COL_START_SIZE)
		buffer(POLY_BACK_COL_START, POLY_BACK_COL_START_SIZE);
	else
		buffer(POLYGON_COLOUR_START, POLYGON_COLOUR_START_SIZE);

	(void)formatindex(index,TRUE);

	if (index == 0 && POLY_BACK_COL_TERM_SIZE)
		buffer(POLY_BACK_COL_TERM, POLY_BACK_COL_TERM_SIZE);
	else
		buffer(POLYGON_COLOUR_TERM, POLYGON_COLOUR_TERM_SIZE);

	/*
	 * This is a hack since PostScript
	 * Poly_Colour command is the same
	 * as the Line_Colour.
	 */
	LINE_COLOUR_DAMAGE = TRUE;

	return(OK);
}

/*
 *	Takes a line width and generates the commands to change the 
 *	line colour.
 */ 
Ct_err	setlinewidth(width)
float	width;
{

	if (LINE_WIDTH_START_SIZE == 0)
		return(OK);

	buffer(LINE_WIDTH_START, LINE_WIDTH_START_SIZE);

	width *= 8.0 * LINE_WIDTH_SCALE;

	if ((int)width < LINE_WIDTH_MIN)
		(void)formatwidth(LINE_WIDTH_MIN);
	else if ((int)width > LINE_WIDTH_MAX)
		(void)formatwidth(LINE_WIDTH_MAX);
	else
		(void)formatwidth((int)width);

	buffer(LINE_WIDTH_TERM, LINE_WIDTH_TERM_SIZE);

	return(OK);
}

/*
 *	Takes a line style and changes the style in the default table
 *	This is all that is necessary since the we generate the line styles
 *	in software.
 */ 
Ct_err	setlinestyle(style)
int	style;
{
	LINE_TYPE = style;
}

/*
 *	Below are the functions called from the jumptable
 *
 * 	Class 0 Function
 */
/*ARGSUSED*/
Ct_err	BegMF(c)
CGMC *c;
{

	CoordRect	dev_extent;
	CoordModifier	coord_mod;
	DCtype		x_extent;
	
	/*
	 * 	Init translation values and the formating routines
	 */
	dev_extent.llx = LOWER_LEFT_X;
	dev_extent.lly = LOWER_LEFT_Y;
	dev_extent.ury = UPPER_RIGHT_Y;
	dev_extent.urx = UPPER_RIGHT_X;

	VDCExtent.llx = XMIN;
	VDCExtent.lly = YMIN;
	VDCExtent.urx = YMAX;
	VDCExtent.ury = YMAX;

	coord_mod.x_off = XOFFSET;
	coord_mod.y_off = YOFFSET;
	coord_mod.x_scale = XSCALE;
	coord_mod.y_scale = YSCALE;

	SetDevWin((long) DEV_WIN_LL_X, (long) DEV_WIN_LL_Y, 
				(long) DEV_WIN_UR_X, (long) DEV_WIN_UR_Y);
	transinit(&dev_extent, coord_mod, TRUE);

	formatinit();

	/*
	 *	if this device has raster instructions then init the module
	 */
	if (RASTER_HOR_START_SIZE > 0) {
		dev_extent.llx = RASTER_LOWER_LEFT_X;
		dev_extent.lly = RASTER_LOWER_LEFT_Y;
		dev_extent.urx = RASTER_UPPER_RIGHT_X;
		dev_extent.ury = RASTER_UPPER_RIGHT_Y;

		coord_mod.x_off = RASTER_XOFFSET;
		coord_mod.y_off = RASTER_YOFFSET;
		coord_mod.x_scale = RASTER_XSCALE;
		coord_mod.y_scale = RASTER_YSCALE;

		transinit(&dev_extent, coord_mod, FALSE);

		(void) rasterformatinit();
	}

	/*
	 * if not Batch put the device in graphics mode. Else its up
	 * to the controlling program to initialize the device for graphics
	 */
	if (!Batch) {
		buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);
		/*
		 * clear the display
		 */
		(void)buffer(ERASE, ERASE_SIZE);
	}
		

	/*
	 * allocate memory for coordinate buffer
	 */
	if (coordBuf) cfree ((char *) coordBuf);
	coordBuf = (Ptype *) icMalloc (1024 * (unsigned) sizeof(Ptype));
	coordBufSize = 1024;


	/*
	 * if device is incapable of drawing filled polygons or they
	 * are too big than use software simulation
	 */
	fillScaleFactor = YScale(POLY_SIM_SPACING) + 1;
	fillScaleFactor = fillScaleFactor == 0 ? 1 : fillScaleFactor;
	
	x_extent = (DCtype) MAX(UPPER_RIGHT_X, LOWER_LEFT_X);
	initSoftSim((DCtype) ABS(UPPER_RIGHT_Y - LOWER_LEFT_Y) 
			/ fillScaleFactor,
			x_extent);


#ifdef	DEAD
	/*
	 * overide graphcap DEVICE_BATCH if main driver batch is true
	 */
	BATCH = Batch ? Batch : BATCH;
#endif

	deviceIsInit = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err	EndMF(c)
CGMC *c;
{

	if (!deviceIsInit) {
		return(OK);
	}

	/*
	 *	reset back to text mode
	 */
	(void)buffer(TEXT_INIT, TEXT_INIT_SIZE);

	flush();

	if (coordBuf) cfree ((char *) coordBuf);
	coordBuf = NULL;

	deviceIsInit = FALSE;
	return (OK);
}


/*ARGSUSED*/
Ct_err	BegPic(c)
CGMC *c;
{
	int	i,j,k;		/* loop var */
	long	data[3];


	/*
	 * 	init the colour map
	 */
	if (COLOUR_AVAIL && MAP_AVAIL) {

		if (!MAP_INDIVIDUAL)
			for (i=0;i<MAP_START_SIZE;i++) 
				switch (MAP_START[i]) {
				case (char)MAD:
					(void)formatindex((CItype)MAP_INIT_INDEXS[0],
							  FALSE);
					break;
				default: 
					buffer(&MAP_START[i],1);
					break;
				}

		for(i=0,j=0;j<MAP_INDEX_DEFINED;j++) {

			if (MAP_INDIVIDUAL)
				for (k=0;k<MAP_START_SIZE;k++) 
					switch (MAP_START[k]) {
					case (char)MAD:
						(void)formatindex(
							(CItype)MAP_INIT_INDEXS[j],
							FALSE);
						break;
					default: 
						buffer(&MAP_START[k],1);
						break;
					}

			switch (MAP_MODEL) {
			case	0:	/* gray scale	*/
				data[0] = MAP_INIT[i];
				i += 1;
				break;
			case	1:	/* rgb	*/
			case	2:	/* bgr	*/
			case	3:	/* hls	*/
				data[0] = MAP_INIT[i];
				data[1] = MAP_INIT[i+1];
				data[2] = MAP_INIT[i+2];
				i += 3;
				break;
			default:
				ct_error(NT_NULL,"bad graphcap color model");
				break;
			}

			(void)formatintensity(data, MAP_MODEL == 0 ? 1 : 3);

			if (MAP_INDIVIDUAL)
				buffer(MAP_TERM,MAP_TERM_SIZE);
		}

		if (!MAP_INDIVIDUAL)
			buffer(MAP_TERM,MAP_TERM_SIZE);
	}

	SetInPic((boolean)TRUE);

	FILL_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;
	LINE_WIDTH_DAMAGE = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err	BegPicBody(c)
CGMC *c;
{
	/*
	 * map virtual coords to virtual virtual coords
	 */
	GetDevWin(&deviceWinCoord);

	return (OK);
}

/*ARGSUSED*/
Ct_err	EndPic(c)
CGMC *c;
{

	(void)flush();

	/*
	 * if not a batch device prompt user before advancing.
	 */
	if (!BATCH) {
		(void)buffer(CURSOR_HOME, CURSOR_HOME_SIZE);
		(void)buffer(USER_PROMPT,USER_PROMPT_SIZE);
		(void)flush();

		/*
		 * if not Batch wait for user to hit return
		 */
		if (!Batch) {
			(void)lseek(0,0L,L_SET);
			while (getc(tty) != '\n');	
			(void)lseek(0,0L,L_SET);
			(void)buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);
		}

	}
	/*
	 * Clear the display
	 */
	(void)buffer(ERASE, ERASE_SIZE);


	/*
	 * reset to default attributes
	 */
	(void)SetInPic((boolean)FALSE);
	return (OK);
}

Ct_err	PolyLine(c)
CGMC *c;
{
	long	x1,y1,x2,y2;	/* the clipped line coords */
	register long	n,p;

	extern	Ct_err	Instr_Dec();

	/*
	 *	Make sure the line attributes are set
	 */
	if (COLOUR_AVAIL && LINE_COLOUR_DAMAGE) {
		(void)setlinecolour(LINE_COLOUR.index);
		LINE_COLOUR_DAMAGE = FALSE;
	}

	if ((LINE_WIDTH_START_SIZE > 0) && LINE_WIDTH_DAMAGE) {
		(void)setlinewidth(LINE_WIDTH);
		LINE_WIDTH_DAMAGE = FALSE;
	}

	if (CLIP_DAMAGE) {
		gcap_set_clip(deviceWinCoord, VDCExtent, CLIPFLAG);
		CLIP_DAMAGE = FALSE;
	}


	/*
	 * if no clipping use the quick algorithm
	 */


	if (!CLIPFLAG && !devWinSet) {
	p = 0;
	while (1) {
		n = 0;

		/* Draw lines in groups of (coordBufSize), except for the
		 * last group. n = count of processed points .
		 * p = count of processed unsent point specifications.
		 */
		while (n < c->Pnum) {
			coordBuf[p] = c->p[n];
			n++;

			p++;
			if (p == coordBufSize || p == POLY_MAX_POINTS) {
				coordBufNum = p;
				PointFlush(FALSE,FALSE);

				coordBuf[0] =  coordBuf[p-1];
				p = 1;
			}
		}
		/*
		* see if more flag is set. If so get more data
		*/
		if (c->more) {
			if (Instr_Dec(c) != OK) return (pre_err);
			}
		else break;     /* leave loop   */

	}	/* while (1)	*/
	}	/* if clip	*/

	else {
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

				if (p == coordBufSize || p == POLY_MAX_POINTS){
					coordBufNum = p;
					PointFlush(FALSE,FALSE);

					coordBuf[0] =  coordBuf[p-1];
					p = 1;
				}
				if (c->p[n+1].x != x2 || c->p[n+1].y != y2) {
					coordBuf[p].x = x2;
					coordBuf[p].y = y2;

					coordBufNum = p+1;
					PointFlush(FALSE,FALSE);

					p = 0;
				}
			}
			n++;
		}
		/*
		* see if more flag is set. If so get more data
		*/
		if (c->more) {
			if (Instr_Dec(c) != OK) return (pre_err);

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
	}

	if (LINE_WIDTH == 0.0) 
		return(OK);	/* do nothing if line is zero width	*/

	if (p > 1) {
		coordBufNum = p;
		PointFlush(FALSE, FALSE);
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	DisjtLine(c)
CGMC *c;
{
	return (OK);
}

Ct_err	PolyMarker(c)
CGMC *c;
{
	int	offset;
	int	i;

	CItype	line_colour;
	float	line_width;
	int	line_type;

	VDCtype x,y;
	VDCtype len;


	line_colour = LINE_COLOUR.index;
	line_width = LINE_WIDTH;
	line_type = LINE_TYPE;

	(void)setlinecolour(MARKER_COLOUR.index);
	(void)setlinewidth(1.0);
	(void)setlinestyle(1);

	if (CLIP_DAMAGE) {
		gcap_set_clip(deviceWinCoord, VDCExtent, CLIPFLAG);
		CLIP_DAMAGE = FALSE;
	}

	offset = MARKER_SIZE / 2;

	switch (MARKER_TYPE) {
	case MARKER_X:	
		for(i=0;i<c->Pnum;i++) {
			line( c->p[i].x - offset, c->p[i].y - offset,
			      c->p[i].x + offset, c->p[i].y + offset);

			line( c->p[i].x - offset, c->p[i].y + offset,
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
			line( c->p[i].x - offset, c->p[i].y,
			      c->p[i].x + offset, c->p[i].y);

			line( c->p[i].x, c->p[i].y + offset,
			      c->p[i].x, c->p[i].y - offset);

			line( c->p[i].x - offset, c->p[i].y - offset,
			      c->p[i].x + offset, c->p[i].y + offset);

			line( c->p[i].x - offset, c->p[i].y + offset,
			      c->p[i].x + offset, c->p[i].y - offset);
		}
		break;
	case MARKER_PLUS:
		for(i=0;i<c->Pnum;i++) {
			line( c->p[i].x - offset, c->p[i].y,
			      c->p[i].x + offset, c->p[i].y);

			line( c->p[i].x, c->p[i].y + offset,
			      c->p[i].x, c->p[i].y - offset);
		}
		break;
	case MARKER_DOT:
		for(i=0;i<c->Pnum;i++) {
			/*
			 * see if device needs a larger then normal dot
			 */
			if (!MARKER_DOT_SIZE && dt->markersize == 1.0) 
				line(c->p[i].x,c->p[i].y,c->p[i].x,c->p[i].y);

			else {	/* fat dot - draw a box not a dot	*/
				len = 0.2 * offset;
				x = c->p[i].x - len;
				y = c->p[i].y - len;
					
				len *= 2;	/* dimension of box (dot)*/

				line(x,y,x, y+len);
				line(x,y+len, x+len, y+len);
				line(x+len, y+len, x+len, y);
				line(x+len, y, x, y);
			}
		}
				
		break;
	default:
		ct_error(NT_UPMT,"");
		return (SICK);
		break;
	}

	(void)setlinecolour(line_colour);
	(void)setlinewidth(line_width);
	(void)setlinestyle(line_type);

	return (OK);
}

/* Text function now in text.c */

/*ARGSUSED*/
Ct_err	RestrText(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	ApndText(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	Polygon(c)
CGMC *c;
{
	int	i;		/* loop variable */
	long	x1,y1,x2,y2;	/* the clipped line coords */

	boolean simulate = POLYGON_SIMULATE;	/* use software to 
						 * simulate poly */

	boolean	draw = FALSE;

        long    num_points = 0; /* number of points to process          */

	extern	long	clipxmax, clipxmin, clipymax, clipymin;

	extern	Ct_err	Instr_Dec();



	if (CLIP_DAMAGE) {	/* has clipping changed	*/
		gcap_set_clip(deviceWinCoord, VDCExtent, CLIPFLAG);
		CLIP_DAMAGE = FALSE;
	}

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

			if ((coordBuf = (Ptype *) 
				icRealloc ((char *) coordBuf, (unsigned) 
				(num_points*sizeof(Ptype)*2))) == NULL ) {

				ct_error(NT_MALLOC, "for poly points");
				return (SICK);
			}

			coordBufSize = num_points * 2;
		}

		/*
		 *	load coordinate buffer with points. Do clipping
		 *	if needed.
		 */
		if (CLIPFLAG || devWinSet) {
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

			if (coordBufNum < 3 || !draw) return(OK);
		} else  {

			for(i = 0; i < c->Pnum; i++) { 
				coordBuf[coordBufNum++] = c->p[i];
			}
		}

		/*
		 * see if more flag is set. If so get more data
		 */
		if (c->more) {
			if (Instr_Dec(c) != OK) return (pre_err);
		}
		else break;	/* leave loop	*/
	}

	/*
	 * if device has no hardware support for polgons or device has 
	 * poly hardware but can't handle that many points and user wants
	 * simulation done in this case  => simulate polygon
	 * Or if -softfill option given on command line
	 */
	if ((*softFill)||
		(((!POLYGON_START_SIZE)||(coordBufNum>POLY_MAX_POINTS))
		&& simulate)) {

			sim_polygon(coordBuf, (int) coordBufNum);
			coordBufNum = 0;
	}
	else {
		if (COLOUR_AVAIL)  {
			
			if (FILL_COLOUR_DAMAGE) {
				(void)setfillcolour(FILL_COLOUR.index);
				FILL_COLOUR_DAMAGE = FALSE;
				PointFlush(TRUE,FALSE);
			} else
				PointFlush(TRUE,FALSE);

		} else
			PointFlush(TRUE,TRUE);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	PolygonSet(c)
CGMC *c;
{
	return (OK);
}

/*
 *	Software simulation of the cell array stuff
 *	Cell arrays are simulated by drawing filled polygons
 */
Ct_err	cellsim(c)
CGMC *c;
{
	float	deltax,deltay;
	float	px,py,rx,ry,qx,qy;
	int	i,j;
	int	nx,ny;

	Ctype	index;

        static  startx = 0,starty = 0;
        static  offset = 0;

	nx = c->i[0];
	ny = c->i[1];

	deltax = (c->p[2].x - c->p[0].x) / nx;
	deltay = (c->p[2].y - c->p[1].y) / ny;

	for(i=starty;i<ny;i++) {
		px = c->p[0].x;
		qx = rx = px + deltax;
		qy = c->p[1].y + (i * deltay);
		py = ry = qy + deltay;

		for(j=startx;j<nx;j++) {


			index = c->c[i*nx+j] - offset;
			if (index > c->Cnum)  {
                                if (c->more) {
                                        startx = j;
                                        starty = i;
                                        offset = c->Cnum + 1;
				}
				return(OK);
			} else {

				(void)setfillcolour((CItype)c->c[index]);
				
				coordBufNum = 4;
				coordBuf[0].x = (int)px;
				coordBuf[0].y = (int)py;
				coordBuf[1].x = (int)rx;
				coordBuf[1].y = (int)ry;
				coordBuf[2].x = (int)qx;
				coordBuf[2].y = (int)qy;
				coordBuf[3].x = (int)px;
				coordBuf[3].y = (int)qy;

				PointFlush(TRUE,FALSE);
			}

			px = rx;
			qx = rx = px + deltax;
		}
		startx = 0;
	}
	starty = 0;
	offset = 0;

	return (OK);
}


/*ARGSUSED*/
Ct_err	CellArray(c)
CGMC *c;
{
	boolean 	clip = FALSE;
	extern	long	clipxmax, clipxmin, clipymax, clipymin;


	if (CLIP_DAMAGE) {
		gcap_set_clip(deviceWinCoord, VDCExtent, CLIPFLAG);
		CLIP_DAMAGE = FALSE;
	}

	if ((c->p[0].y != c->p[2].y) || (c->p[2].x != c->p[1].x)) {
		ct_error(NT_NULL,
			"graphcap ctrans does not accept non rectangular cell arrays");
		return (SICK);
	}

	if (CLIPFLAG || devWinSet) {	/* do we need to do clipping?	*/
		if (c->p[0].x < clipxmin || c->p[0].y < clipymin
		|| c->p[1].x > clipxmax || c->p[1].y > clipymax) 

		clip = TRUE;
	}

	/*
	 *	if the device has raster instructions use them
	 */
	if (RASTER_HOR_START_SIZE > 0 && !clip)
		(void)raster(c);
	/*
	 *	The device has nothing so just draw a box or we need
	 * 	to clip the cell array in which case we punt at this point.
	 */
	else if (!RASTER_SIMULATE || clip) {
		line(c->p[0].x,c->p[0].y,c->p[2].x,c->p[2].y);
		line(c->p[2].x,c->p[2].y,c->p[1].x,c->p[1].y);
		line(c->p[1].x,c->p[1].y,c->p[0].x,c->p[1].y);
		line(c->p[0].x,c->p[1].y,c->p[0].x,c->p[0].y);

	} else  	/* cell array simulation */
		(void)cellsim(c);

	return (OK);
}

/*ARGSUSED*/
Ct_err	GDP(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	Rect(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	Circle(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	Arc3Pt(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	Arc3PtClose(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	ArcCtr(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	ArcCtrClose(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	Ellipse(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	EllipArc(c)
CGMC *c;
{
	return (OK);
}

/*ARGSUSED*/
Ct_err	EllipArcClose(c)
CGMC *c;
{
	return (OK);
}

/*
 * Class 6 Functions
 */

/*ARGSUSED*/
Ct_err	Escape(c)
CGMC *c;
{
	return (OK);
}

/*
 * Class 7 Functions
 */
/*ARGSUSED*/
Ct_err	Message(c)
CGMC *c;
{

	return (OK);
}
/*ARGSUSED*/
Ct_err	ApplData(c)
CGMC *c;
{
	return (OK);
}

/*
 *	The following Colour Table function has been moved from default.c
 *	to gcap.c because it's implementation is graphcap specific.
 */

Ct_err	ColrTable(c)
CGMC *c;
{
	int	i,k;		/* loop var */
	long	data[3];


	if (!COLOUR_AVAIL)
		return(OK);
	if (!MAP_AVAIL)
		return(OK);

	/*
	 * make sure color table does not exceed device capabilities
	 */
	if (c->ci[0] >= MAP_INDEX_MAX) 
		return(OK);

	if ((c->ci[0] + c->CDnum - 1) > MAP_INDEX_MAX) {
		c->CDnum = MAP_INDEX_MAX - c->ci[0];
	}

	if (!MAP_INDIVIDUAL)

		for (k=0;k<MAP_START_SIZE;k++) 
			switch (MAP_START[k]) {
			case (char)MAD:
				(void)formatindex(c->ci[0],FALSE);
				break;
			default: 
				buffer(&MAP_START[k],1);
				break;
			}

	for(i=0;i<c->CDnum;i++) {

		if (MAP_INDIVIDUAL)
			for (k=0;k<MAP_START_SIZE;k++) 
				switch (MAP_START[k]) {
				case (char)MAD:
					(void)formatindex(c->ci[0],FALSE);
					break;
				default: 
					buffer(&MAP_START[k],1);
					break;
				}

		switch (MAP_MODEL) {
		case 0: /* mono */
			data[0] =	(0.3 * c->cd[i].red) + 
					(0.6 * c->cd[i].green) +
					(0.1 * c->cd[i].blue);

			(void)formatintensity(data, 1);
			break;
		case 1:	/* RGB	*/
			data[0] = c->cd[i].red;
			data[1] = c->cd[i].green;
			data[2] = c->cd[i].blue;

			(void)formatintensity(data, 3);
			break;
		case 2:	/* BGR	*/
			data[0] = c->cd[i].blue;
			data[1] = c->cd[i].green;
			data[2] = c->cd[i].red;

			(void)formatintensity(data, 3);
			break;
		case  3: /* HLS */
			RGBtoHLS ( c->cd[i].red, 
				   c->cd[i].green, 
				   c->cd[i].blue,
				   &data[0],
				   &data[1],
				   &data[2]);

			(void)formatintensity(data, 3);
			break;
		default:
			ct_error(NT_NULL, "bad graphcap colour map model");
			break;
		}

		if (MAP_INDIVIDUAL)
			buffer(MAP_TERM,MAP_TERM_SIZE);
	}

	if (!MAP_INDIVIDUAL)
		buffer(MAP_TERM,MAP_TERM_SIZE);

	return (OK);
}

#ifdef	DEAD

/*
 *	circle
 *	internal
 *
 *	draw a circle with center at xc, yc of radius radius
 *	This code comes from Bresenham's algorithm
 */
static	circle(xc,yc,radius)
	int	xc, yc, radius;
{
	int	x, y, d;

	y = radius;
	d = 3 - (2 * radius);

	for (x = 0; x < y;) {
		symmetry(x, y, xc, yc);
		if (d < 0) 
			d += 4 * x + 6;
		else {
			d += 4 * (x - y) + 10;
			y--;
		}
		x++;
	}

}

static	symmetry(x, y, xc, yc) 
	int	x, y, xc, yc;
{
	int	x_start, x_end, x_out;
	int	y_start, y_end, y_out;

	x_start = x;
	x_end = x + 1;
	y_start = y;
	y_end = y + 1;

	for (x_out = x_start; x_out < x_end; ++x_out) {
		line(x_out + xc, y + yc, x_out + xc, y + yc);
		line(x_out + xc, -y + yc, x_out + xc, -y + yc);
		line(-x_out + xc, -y + yc, -x_out + xc, -y + yc);
		line(-x_out + xc, y + yc, -x_out + xc, y + yc);
	}
	for (y_out = y_start; y_out < y_end; ++y_out) {
		line(y_out + xc, x + yc, y_out + xc, x + yc);
		line(y_out + xc, -x + yc, y_out + xc, -x + yc);
		line(-y_out + xc, -x + yc, -y_out + xc, -x + yc);
		line(-y_out + xc, x + yc, -y_out + xc, x + yc);
	}
}

#endif
	
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
		line(x1, y1, x2, y2);
	}
}
