/*
 *	$Id: gcap.c,v 1.7 1991-03-12 17:37:24 clyne Exp $
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

extern	FILE	*tty;
extern	short	devWinSet;
extern	long	lseek();
extern	Ct_err	formatintensity();
extern	Ct_err	formatveccnt();
extern	Ct_err	formatcoord();
extern	Ct_err	formatindex();
extern	Ct_err	formatwidth();
extern	Ct_err	raster();
extern	boolean	Batch;
extern	boolean	deviceIsInit;

static	CoordRect	VDCExtent;

extern	boolean	*softFill;

/*
 * defines for the markers in the strings
 */
#define MAD	-6	/* Colour Index */

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

	int	fill_scale_factor;
	DCtype	x_extent;

	extern	int	commFillScaleFactor;
	
	/*
	 * 	Init translation values and the formating routines
	 */
	dev_extent.llx = LOWER_LEFT_X;
	dev_extent.lly = LOWER_LEFT_Y;
	dev_extent.ury = UPPER_RIGHT_Y;
	dev_extent.urx = UPPER_RIGHT_X;

	VDCExtent.llx = XMIN;
	VDCExtent.lly = YMIN;
	VDCExtent.urx = XMAX;
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
	 * if device is incapable of drawing filled polygons or they
	 * are too big than use software simulation
	 */
	fill_scale_factor = YScale(POLY_SIM_SPACING) + 1;
	fill_scale_factor = fill_scale_factor == 0 ? 1 : fill_scale_factor;
	
	x_extent = (DCtype) MAX(UPPER_RIGHT_X, LOWER_LEFT_X);
	initSoftSim((DCtype) ABS(UPPER_RIGHT_Y - LOWER_LEFT_Y) 
			/ fill_scale_factor,
			x_extent);

	/*
	 * change the fill scale factor used by ComPolySim in commondev.c
	 */
	commFillScaleFactor = fill_scale_factor;

	/*
	 * tweek soft fill option to do software filling if device has no
	 * fill capability and POLYGON_SIMULATE is set
	 */
	if (POLYGON_START_SIZE == 0 && POLYGON_SIMULATE) {
		*softFill = TRUE;
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
		if (!BATCH) {	/* don't clear batch devices to start */
			(void)buffer(ERASE, ERASE_SIZE);
		}
	}

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
	if (MAP_AVAIL) {

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
		if (!Batch && tty) {
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


/*ARGSUSED*/
Ct_err	DisjtLine(c)
CGMC *c;
{
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

	Ptype	coord_buf[4];
	long	coord_buf_num;

	Ctype	index;

        static  startx = 0,starty = 0;
        static  offset = 0;

	void	gcap_pointflush();

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

				gcap_fillcolour((CItype)c->c[index]);
				
				coord_buf_num = 4;
				coord_buf[0].x = (int)px;
				coord_buf[0].y = (int)py;
				coord_buf[1].x = (int)rx;
				coord_buf[1].y = (int)ry;
				coord_buf[2].x = (int)qx;
				coord_buf[2].y = (int)qy;
				coord_buf[3].x = (int)px;
				coord_buf[3].y = (int)qy;

				gcap_pointflush(coord_buf, 
					&coord_buf_num,TRUE,FALSE);
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
                CoordRect       device_win_coord;
                GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, VDCExtent, CLIPFLAG);
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
		gcap_line(c->p[0].x,c->p[0].y,c->p[2].x,c->p[2].y);
		gcap_line(c->p[2].x,c->p[2].y,c->p[1].x,c->p[1].y);
		gcap_line(c->p[1].x,c->p[1].y,c->p[0].x,c->p[1].y);
		gcap_line(c->p[0].x,c->p[1].y,c->p[0].x,c->p[0].y);

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

