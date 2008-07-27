/*
 *	$Id: gcap.c,v 1.46 2008-07-27 03:18:43 haley Exp $
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
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include <ncarg/c.h>
#include "cgmc.h"
#include "graphcap.h"
#include "default.h"
#include "ctrandef.h"
#include "soft_fill.h"
#include "translate.h"
#include "format.h"
#include "gcapdev.h"

/*
**      clamp a value between `MIN' and `MAX'
*/
#define CLAMP(VAL,MIN,MAX)       \
			VAL = VAL < (MIN) ? (MIN) : VAL; \
			VAL = VAL > (MAX) ? (MAX) : VAL;


FILE	*tty = (FILE *) NULL;
extern	boolean	Batch;
extern	boolean	deviceIsInit;
extern	int	optionDesc;
extern	boolean	startedDrawing;

static	struct	GCapOpts_	{
	char	*window;
	char	*viewport;
	boolean	sim_bg;
	char	*outfile;
	} gcap_opts;

static	Option	options[] = {
        {"window", NCARGCvtToString,
		(Voidptr) &gcap_opts.window, sizeof (gcap_opts.window)
	},
	{"viewport", NCARGCvtToString,
		(Voidptr) &gcap_opts.viewport, sizeof (gcap_opts.window)
	},
	{"simulatebg", NCARGCvtToBoolean,
		(Voidptr) &gcap_opts.sim_bg, sizeof (gcap_opts.sim_bg)
	},
	{"outfile", NCARGCvtToString,
		(Voidptr) &gcap_opts.outfile, sizeof (gcap_opts.outfile)
	},
	{
	NULL
	}
};

extern	boolean	*softFill;
boolean	doSimulateBG = FALSE;	/* simulate background color changes ?	*/


/*
 * 	load the default color palette supplied by the graphcap. 
 *
 *	N.B.
 *	It would be nice to simply convert the graphcap-supplied color palette
 *	into a CGM instruction - as we do with the -pal command-line
 *	option - however the graphcap-supplied color palette is already
 *	in a format relative to the color model supported by the device.
 */
#define MAD	-6	/* Colour Index */
int	load_gcap_default_pal()
{
	int	i,j,k;
	long	data[3];
	SignedChar	s_char_;
	int		status = 0;


	if (! MAP_AVAIL) return (0);


	if (! MAP_INDIVIDUAL) {
		for (i=0;i<MAP_START_SIZE;i++) { 

			s_char_ = (SignedChar) MAP_START[i];

			switch ((int) s_char_) {
			case MAD:
				(void)
				formatindex((CItype)MAP_INIT_INDEXS[0], FALSE);

				break;
			default: 
				buffer(&s_char_,1);
				break;
			}
		}
	}

	for(i=0,j=0;j<MAP_INDEX_DEFINED;j++) {

		if (MAP_INDIVIDUAL) {
			for (k=0;k<MAP_START_SIZE;k++) {
				s_char_ = (SignedChar) MAP_START[k];
				switch ((int) s_char_) {
				case MAD:
					(void) formatindex(
						(CItype)MAP_INIT_INDEXS[j],
						FALSE
					);
						break;
				default: 
					buffer(&s_char_,1);
					break;
				}
			}
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
			ESprintf(
				E_UNKNOWN,"Invalid graphcap color model"
			);
			status = -1;
			break;
		}

		(void)formatintensity(data, MAP_MODEL == 0 ? 1 : 3);

		if (MAP_INDIVIDUAL) buffer(MAP_TERM,MAP_TERM_SIZE);
	}

	if (! MAP_INDIVIDUAL) buffer(MAP_TERM,MAP_TERM_SIZE);

	return(status);
}


int	gcap_graphics_mode_(on_off)
	boolean	on_off;
{
	static	boolean	first = TRUE;

	if (on_off) {       /* put device in graphics mode  */

		/*
		 * put the device in graphics mode. If the device is 
		 * a batch device we only put it in graphics mode once
		 * and we don't clear it when we do.
		 */
		if (!BATCH || first) {
			(void) buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);
			if (first && MAP_INDEX_DEFINED > 0) {

				/*
				 * The graphcap has supplied its own 
				 * colormap so load  it and tell ctrans not 
				 * to try and supply its own default
				 * colormap.
				 */
				(void) load_gcap_default_pal();
				_CtDefNoColorDefault();

			}
			first = FALSE;
		}
		if (!BATCH) {   /* don't clear batch devices */
			(void)buffer(ERASE, ERASE_SIZE);
		}
	}

	else {  /* put device in text mode      */

		/*
		 * batch devices don't get cleared and only 
		 * get put into text mode at the end of the 
		 * metafile
		 */
		if (!BATCH) {
			(void)buffer(ERASE, ERASE_SIZE);
			(void)buffer(TEXT_INIT, TEXT_INIT_SIZE);
		}

	}
	(void) flush(); /* send instruction to device   */

	return(0);
}



/*
 *	Below are the functions called from the jumptable
 *
 * 	Class 0 Function
 */
/*ARGSUSED*/
int	BegMF(c)
CGMC *c;
{

	char    *tty_in = "/dev/tty";
	int	status = 0;

	startedDrawing = FALSE;

	/*
	 * parse gcap specific options
	 */
	if (GetOptions(optionDesc, options) < 0) {
		ESprintf(
			E_UNKNOWN,"GetOptions(%d,) [ %s ]",
			optionDesc, ErrGetMsg()
		);
		return(-1);
	}

	if (GcapOpenBuffer(gcap_opts.outfile) < 0) {
		ESprintf(
			E_UNKNOWN,"Opening output file(%s,) [ %s ]",
			gcap_opts.outfile, ErrGetMsg()
		);
		return(-1);
	}
	


	doSimulateBG = gcap_opts.sim_bg;
	
	/*
	 * set device viewport specification
	 */
	if (gcap_opts.viewport) {
		int	llx, lly, urx, ury;

		if (CoordStringToInt(gcap_opts.viewport,&llx,&lly,&urx,&ury)<0){
			ESprintf(
				E_UNKNOWN, 
				"Invalid viewport format [ %s ]", ErrGetMsg()
			);
			status = -1;
		}
		else {
			SetDevViewport(
				(long) llx, (long) lly,(long) urx,(long) ury
			);
		}
	}

	/*
	 * set device window specification from command line if given. Else
	 * get information from the graphcap.
	 */
	if (gcap_opts.window) {
		int     llx, lly, urx, ury;

		if (CoordStringToInt(gcap_opts.window,&llx,&lly,&urx,&ury)<0){
			ESprintf(
				E_UNKNOWN, 
				"Invalid window format [ %s ]", ErrGetMsg()
			);
			status = -1;
		}
		else {
			SetDevWin((long) llx, (long) lly,(long) urx,(long) ury);
		}
	}
	else {
		SetDevWin(
			(long) DEV_WIN_LL_X, (long) DEV_WIN_LL_Y, 
			(long) DEV_WIN_UR_X, (long) DEV_WIN_UR_Y
		);
	}


	formatinit();


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
		gcap_graphics_mode_(1);
	}

	/*
	 * if not in batch mode open a tty to prompt user for frame 
	 * advances
	 */
	if (!Batch && !BATCH) {
		if (! (tty = fopen(tty_in, "r"))) {
			ESprintf(errno,"fopen(%s, r)", tty_in);
			return(-1);
		}
	}

	deviceIsInit = TRUE;
	return (status);
}

/*ARGSUSED*/
int	EndMF(c)
CGMC *c;
{

	if (!deviceIsInit) {
		return(0);
	}

	if (tty) (void) fclose(tty);


	/*
	 *	reset back to text mode
	 */
	(void)buffer(TEXT_INIT, TEXT_INIT_SIZE);

	flush();

	deviceIsInit = FALSE;
	return (0);
}


/*ARGSUSED*/
int	BegPic(c)
CGMC *c;
{
	int	status = 0;

	SetInPic((boolean)TRUE);

	FILL_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;
	LINE_WIDTH_DAMAGE = TRUE;

	if (gcap_opts.sim_bg) {
		/*
		 * force painting of background
		 */
		COLOUR_TOTAL_DAMAGE++;
		COLOUR_INDEX_DAMAGE(0) = TRUE;	/* background color index */
	}

		
	return (status);
}

/*ARGSUSED*/
int	BegPicBody(c)
CGMC *c;
{
	CoordRect	dev_extent;
	CoordModifier	coord_mod;

	extern	int	commFillScaleFactor;
	extern	int	commHatchScaleFactor;

	if (VDC_EXTENT_DAMAGE) {
		/*
		 * 	Init translation values and the formating routines
		 */
		dev_extent.llx = LOWER_LEFT_X;
		dev_extent.lly = LOWER_LEFT_Y;
		dev_extent.ury = UPPER_RIGHT_Y;
		dev_extent.urx = UPPER_RIGHT_X;
		coord_mod.x_off = XOFFSET;
		coord_mod.y_off = YOFFSET;
		coord_mod.x_scale = XSCALE;
		coord_mod.y_scale = YSCALE;

		transinit(&dev_extent, coord_mod, TRUE);
		/*
		 *	if this device has raster instructions then 
		 *	init the module
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
		commFillScaleFactor = commFillScaleFactor == 0 ? 
						1 : commFillScaleFactor;

		/*
		 * divide hatch spacing by 2 to make things consistent with 
		 * ftrans output
		 */
		commHatchScaleFactor = YScale(POLY_HATCH_SPACE) / 2;
		commHatchScaleFactor = commHatchScaleFactor==0 ?
						1 : commHatchScaleFactor;
		if (initSoftSim(
			(DCtype) XConvert(XMIN),
			(DCtype) XConvert(XMAX),
			(DCtype) YConvert(YMIN),
			(DCtype) YConvert(YMAX)
			) < 0) {

		return(-1);
		}

		VDC_EXTENT_DAMAGE = FALSE;
	}

	startedDrawing = FALSE;
	return (0);
}

/*ARGSUSED*/
int	EndPic(c)
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
			(void)lseek(0,0L,SEEK_SET);
			while (getc(tty) != '\n')
				;	
			(void)lseek(0,0L,SEEK_SET);
		}
		(void)buffer(GRAPHIC_INIT, GRAPHIC_INIT_SIZE);

	}


	/*
	 * reset to default attributes
	 */
	(void)SetInPic((boolean)FALSE);
	return (0);
}

/*ARGSUSED*/
int	ClearDevice(c)
CGMC *c;
{
	(void)buffer(ERASE, ERASE_SIZE);
	return (0);
}


/* Text function now in text.c */

/*ARGSUSED*/
int	RestrText(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	ApndText(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}


/*ARGSUSED*/
int	PolygonSet(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*
 *	Software simulation of the cell array stuff
 *	Cell arrays are simulated by drawing filled polygons
 */
int	cellsim(c)
CGMC *c;
{
	double	delta_r_x, delta_r_y;	/* step from P to R	*/
	double	delta_q_x, delta_q_y;	/* step from P to Q	*/
	double	delta_z_x, delta_z_y;	/* step from P to Z	*/
	Ptype	p, q, r;	/* cell array corners		*/
	Ptype	z;		/* the fourth corner		*/
	int	i,j;
	int	nx,ny;
	double	x0, y0, x1, y1, x2, y2, x3, y3;
	double	px, py;

	Ptype	coord_buf[4];
	long	coord_buf_num;

	Ctype	color_index;	/* color index of the current cell	*/
	int	cgmc_index;	/* index into the cgmc color list	*/

	extern void	gcap_pointflush(), gcap_fillcolour();
	extern	long	clipxmax, clipxmin, clipymax, clipymin;

	nx = c->i[0];
	ny = c->i[1];
	p = c->p[0];
	q = c->p[1];
	r = c->p[2];

	/*
	 * are the nx cells layed out vertically or horizontally? Calculate
	 * the step increments to go from a Pi to a Qi, Ri, and Zi.
	 */
	if (p.x == r.x) {	/* vertically	*/
		z.x = q.x;
		z.y = p.y;
		delta_r_x = 0;
		delta_r_y = (double) (r.y - p.y) / (double) nx;
		delta_z_x = (double) (z.x - p.x) / (double) ny;
		delta_z_y = 0;
		delta_q_x = delta_z_x;
		delta_q_y = delta_r_y;
	}
	else {
		z.x = p.x;
		z.y = q.y;
		delta_r_x = (double) (r.x - p.x) / (double) nx;
		delta_r_y = 0;
		delta_z_x = 0;
		delta_z_y = (double) (z.y - p.y) / (double) ny;
		delta_q_x = delta_r_x;
		delta_q_y = delta_z_y;
	}
	
	px = (double) p.x;
	py = (double) p.y;

	for(i=0, cgmc_index=0; i<ny; i++) {

		/*
		 * the coords of the first cell in row i
		 */
		x0 = px;
		y0 = py;
		x1 = px + delta_r_x;
		y1 = py + delta_r_y;
		x2 = px + delta_q_x;
		y2 = py + delta_q_y;
		x3 = px + delta_z_x;
		y3 = py + delta_z_y;

		for(j=0;j<nx;j++) {


			/* make sure data available in cgmc     */
			if (cgmc_index >= c->Cnum) {
				if (c->more) {
					if (Instr_Dec(c) < 1) {
						return(-1);
					}
					cgmc_index = 0;
				}
				else {
					ESprintf(
						E_UNKNOWN, "Cell array encoding"
					);
					return(-1);
				}
			}

			color_index = c->c[cgmc_index];
			cgmc_index++;

			gcap_fillcolour(color_index);
				

			/*
			 * convert back to ints
			 */
			coord_buf[0].x = (int) x0;
			coord_buf[0].y = (int) y0;
			coord_buf[1].x = (int) x1;
			coord_buf[1].y = (int) y1;
			coord_buf[2].x = (int) x2;
			coord_buf[2].y = (int) y2;
			coord_buf[3].x = (int) x3;
			coord_buf[3].y = (int) y3;

			/*
			**	clip the cell
			*/
			CLAMP(coord_buf[0].x,clipxmin,clipxmax);
			CLAMP(coord_buf[0].y,clipymin,clipymax);
			CLAMP(coord_buf[1].x,clipxmin,clipxmax);
			CLAMP(coord_buf[1].y,clipymin,clipymax);
			CLAMP(coord_buf[2].x,clipxmin,clipxmax);
			CLAMP(coord_buf[2].y,clipymin,clipymax);
			CLAMP(coord_buf[3].x,clipxmin,clipxmax);
			CLAMP(coord_buf[3].y,clipymin,clipymax);

			coord_buf_num = 4;	/* need to reset each time */
			gcap_pointflush(coord_buf, &coord_buf_num,TRUE,FALSE);

			/*
			 * move to the next cell
			 */
			x0 = x1;
			y0 = y1;
			x3 = x2;
			y3 = y2;
			x1 = x0 + delta_r_x;
			y1 = y0 + delta_r_y;
			x2 = x0 + delta_q_x;
			y2 = y0 + delta_q_y;

		}

		px = px + delta_z_x;
		py = py + delta_z_y;
	}

	return (0);
}

int	PolyMarker(c)
	CGMC *c;
{
	int	_PolyMarker();

	return(_PolyMarker(c, MARKER_DOT_SIZE));
}



/*ARGSUSED*/
int	CellArray(c)
CGMC *c;
{

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
	int	status = 0;

	int	cell_array(), non_rect_cell_array(), CellArray_();
	extern	long	clipxmax, clipxmin, clipymax, clipymin;

	if (COLOUR_TABLE_DAMAGE) {
		if (gcap_update_color_table() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
                CoordRect       device_win_coord;
                GetDevWin(&device_win_coord);
		gcap_set_clip(device_win_coord, 
			PackCoordRect(XMIN, YMIN, XMAX,YMAX), CLIPFLAG
		);
		CLIP_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
 	 *	extract data from cgmc
	 */


		/*	dimensions	*/
	nx = c->i[0];		ny = c->i[1];


		/*	cell representation mode	*/
	mode = c->e[0];

	if (CSM != MODE_INDEXED) {
		ESprintf(EINVAL, "Direct color not supported");
		(void) MunchCGM(c);
		return (-1);
	}

	if (mode != PACKED_MODE) {
		ESprintf(EINVAL, "Run length encoding not supported");
		(void) MunchCGM(c);
		return(-1);
	}

	if (c->p[0].x < clipxmin || c->p[0].y < clipymin
		|| c->p[0].x > clipxmax || c->p[0].y > clipymax
		|| c->p[1].x < clipxmin || c->p[1].y < clipymin
		|| c->p[1].x > clipxmax || c->p[1].y > clipymax) { 

		clip = TRUE;
	}

        /*
         * see if cell array is rectangular or not
         */
        if (c->p[2].x != c->p[1].x || c->p[0].y != c->p[2].y) {
		ESprintf(EINVAL, "Cell array is non-rectangular");
		(void) MunchCGM(c);
		return(-1);
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
	else if (!RASTER_SIMULATE) {
		gcap_line(c->p[0].x,c->p[0].y,c->p[1].x,c->p[0].y);
		gcap_line(c->p[1].x,c->p[0].y,c->p[1].x,c->p[1].y);
		gcap_line(c->p[1].x,c->p[1].y,c->p[0].x,c->p[1].y);
		gcap_line(c->p[0].x,c->p[1].y,c->p[0].x,c->p[0].y);

	} else {  	/* cell array simulation */
		(void)cellsim(c);
	}

	(void) MunchCGM(c);
	return (status);
}

/*ARGSUSED*/
int	GDP(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	Rect(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	Circle(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	Arc3Pt(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	Arc3PtClose(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	ArcCtr(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	ArcCtrClose(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	Ellipse(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	EllipArc(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	EllipArcClose(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*
 * Class 6 Functions
 */

/*ARGSUSED*/
int	Escape(c)
CGMC *c;
{
	return (0);
}

/*
 * Class 7 Functions
 */
/*ARGSUSED*/
int	Message(c)
CGMC *c;
{

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}
/*ARGSUSED*/
int	ApplData(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

