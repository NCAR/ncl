/*
 *	$Id: gcap.c,v 1.16 1992-02-20 18:49:52 clyne Exp $
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

	extern	int	commFillScaleFactor;
	extern	int	commHatchScaleFactor;

	void	SetDevWin();
	
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
	commFillScaleFactor = YScale(POLY_SIM_SPACING) + 1;
	commFillScaleFactor = commFillScaleFactor == 0? 1 : commFillScaleFactor;

	/*
	 * divide hatch spacing by 2 to make things consistent with 
	 * ftrans output
	 */
	commHatchScaleFactor = YScale(POLY_HATCH_SPACE) / 2;
	commHatchScaleFactor = commHatchScaleFactor==0?1: commHatchScaleFactor;
	
	initSoftSim(
		(DCtype) XConvert(XMIN),
		(DCtype) XConvert(XMAX),
		(DCtype) YConvert(YMIN),
		(DCtype) YConvert(YMAX)
	);


	/*
	 * tweek soft fill option to do software filling if device has no
	 * fill capability and POLYGON_SIMULATE is set
	 */
	if (POLYGON_START_SIZE == 0 && POLYGON_SIMULATE) {
		*softFill = TRUE;
	}

	/*
	 * if not connected to a tty put ourselves in BATCH mode
	 */
	if (! isatty(fileno(stdout))) BATCH = TRUE;

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
				case MAD:
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
					case MAD:
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
			while (getc(tty) != '\n')
				;	
			(void)lseek(0,0L,L_SET);
		}
		(void)buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);

	}


	/*
	 * reset to default attributes
	 */
	(void)SetInPic((boolean)FALSE);
	return (OK);
}

/*ARGSUSED*/
Ct_err	ClearDevice(c)
CGMC *c;
{
	/*
	 * Clear the display
	 */
	(void)buffer(ERASE, ERASE_SIZE);
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

	void	gcap_pointflush(), gcap_fillcolour();

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

Ct_err	PolyMarker(c)
	CGMC *c;
{
	Ct_err	_PolyMarker();

	return(_PolyMarker(c, MARKER_DOT_SIZE));
}



/*ARGSUSED*/
Ct_err	CellArray(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CellArray\n");
#endif DEBUG

#define	PACKED_MODE	1


	/*	
	 *	programmers unfamiliar with CGM representation of Cell arrays
	 *	should see section 5.6.9  in the ANSI document on 
	 *	Computer Graphic Metafiles.
	 */


	/* points giving boundry of cell array	*/
	Ptype	P, Q, R;	/* cell array corner boundries		*/
	int	nx, ny;		/* dimensions of cell array by number of cells*/
	Etype	mode;		/* cell representation mode		*/
	boolean	clip = FALSE;

	Ct_err	cell_array(), non_rect_cell_array(), CellArray_();
	void	gcap_line(), gcap_update_color_table();
	extern	long	clipxmax, clipxmin, clipymax, clipymin;

	if (COLOUR_TABLE_DAMAGE) {
		gcap_update_color_table();
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
                CoordRect       device_win_coord;
                GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, VDCExtent, CLIPFLAG);
		CLIP_DAMAGE = FALSE;
	}

	/*
 	 *	extract data from cgmc
	 */


		/*	dimensions	*/
	nx = c->i[0];		ny = c->i[1];


		/*	cell representation mode	*/
	mode = c->e[0];

	if (CSM != INDEXED) {
		ct_error(NT_CAFE, "direct color not supported");
		(void) MunchCGM(c);
		return (SICK);
	}

	if (mode != PACKED_MODE) {
		(void) fprintf(stderr, 
		"ctrans: run length encoded cell arrays not supported\n");
		(void) MunchCGM(c);
		return(OK);
	}

	if (CLIPFLAG || devWinSet) {	/* do we need to do clipping?	*/
		if (c->p[0].x < clipxmin || c->p[0].y < clipymin
			|| c->p[0].x > clipxmax || c->p[0].y > clipymax
			|| c->p[1].x < clipxmin || c->p[1].y < clipymin
			|| c->p[1].x > clipxmax || c->p[1].y > clipymax) { 

			clip = TRUE;
		}
	}

        /*
         * see if cell array is rectangular or not
         */
        if (c->p[2].x != c->p[1].x || c->p[0].y != c->p[2].y) {
		ct_error(NT_NULL, "non rectangular cell array");
		(void) MunchCGM(c);
		return(OK);
        }

	/*
	 *	if the device has raster instructions use them
	 */
	if (RASTER_HOR_START_SIZE > 0 && !clip) {
		P.x = R_XConvert(c->p[0].x);	P.y = R_YConvert(c->p[0].y);
		Q.x = R_XConvert(c->p[1].x);	Q.y = R_YConvert(c->p[1].y);
		R.x = R_XConvert(c->p[2].x);	R.y = R_YConvert(c->p[2].y);
		return(CellArray_(c, P, Q, R, nx, ny));
	}
	/*
	 *	The device has nothing so just draw a box or we need
	 * 	to clip the cell array in which case we punt at this point.
	 */
	else if (!RASTER_SIMULATE || clip) {
		gcap_line(c->p[0].x,c->p[0].y,c->p[1].x,c->p[0].y);
		gcap_line(c->p[1].x,c->p[0].y,c->p[1].x,c->p[1].y);
		gcap_line(c->p[1].x,c->p[1].y,c->p[0].x,c->p[1].y);
		gcap_line(c->p[0].x,c->p[1].y,c->p[0].x,c->p[0].y);

	} else {  	/* cell array simulation */
		(void)cellsim(c);
	}

	(void) MunchCGM(c);
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

