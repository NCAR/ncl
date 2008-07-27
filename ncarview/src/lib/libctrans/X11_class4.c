/*
 *	$Id: X11_class4.c,v 1.35 2008-07-27 03:18:42 haley Exp $
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
/*	X11_class4.c
 *
 *
 *		Author		John Clyne	
 *
 *	This file contain the functions that implement class 4 
 *	CGM elements. The supported elements are POLYLINE, POLYMARKER,
 *	TEXT, POLYGON, CELL ARRAY and  GENERALIZED DRAWING PRIMITIVE.
 *	These elements all produce output to the screen. 
 *
 * rev 1.01 7/4/90 clyne	: Cell array interpolation performed poorly
 * rev 1.02 8/13/90 clyne	: Partitioned CGM data not handled by Polyline()
 */
/*LINTLIBRARY*/



#include	<stdio.h>
#include	<stdlib.h>
#include	<math.h>
#include	<errno.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarg/c.h>
#include	<ncarg/cgm_tools.h>
#include	"cgmc.h"
#include	"default.h"
#include	"Xdefs.h"
#include	"ctrandef.h"
#include	"Xcrm.h"
#include	"soft_fill.h"
#include	"translate.h"

extern	int	Colordef[];
extern	boolean	*softFill;
extern	Pixeltype	Colortab[];
extern	boolean	Color_ava;
extern	boolean	startedDrawing;

static	struct	{	/* a pixmap for stippling a filled polygon*/
	GC	gc;
	XPoint	P[4];
	Pixmap	stippleid;
	int	width,
		height;
	} stipple = { 0,0, 0,0, 0,0, 0,0, 
			0, 0, 0};	
	

/*
 *	The class 4 CGM elements functions
 */

/*	GCsetcolor:
 *
 *		set the color of a particular GC
 *
 *	on entry
 *		color	: the desired color
 *	on exit
 *		gc	: has foreround attribute set to color	
 */
static	int	GCsetcolor(color, gc)
	COtype	color;
	GC	gc;
{


	/* check COLOUR SELECTION MODE	*/
	if (CSM == MODE_INDEXED) {

		/* see if the color has been defined	*/
		if (Colordef[color.index] > -1) { 
			XSetForeground(dpy, gc, Colortab[color.index]);
		}
		else {
			/* set to default color if invalid index	*/
			XSetForeground(dpy, gc, Colortab[1]);

			ESprintf(EINVAL,"Undefined color index(%d)",color.index);
			return(-1);
		}
	}

	return (0);
}


/*	GCsetlinetype
 *
 *		set the GC line attribute
 *	on entry
 *		linetype: is the desired line attribute
 *	on exit
 *		lineGC	: has line attribute linetype
 */
#define    DASHSIZE	4
#define    DOTSIZE	1
#define    GAPSIZE	4
static	int	GCsetlinetype(linetype)
	IXtype	linetype;
{
	static	const	char	
		dashlist[] = {DASHSIZE,GAPSIZE};

	static	const	char	
		dotlist[] = {DOTSIZE,GAPSIZE};

	static	const	char	
		dashdotlist[] = {DASHSIZE, GAPSIZE,DOTSIZE,GAPSIZE};

	static	const	char	
		dashddlist[] = {
			DASHSIZE,GAPSIZE,DOTSIZE, GAPSIZE,DOTSIZE,GAPSIZE
		};

	if (linetype == L_SOLID)
		gcv.line_style = LineSolid;

	else {
		switch(linetype) {
		case L_DASH	:
			XSetDashes(dpy, lineGC, 0, dashlist, sizeof(dashlist));
			break;

		case L_DOT	:
			XSetDashes(dpy, lineGC, 0, dotlist, sizeof(dotlist));
			break;

		case L_DASH_DOT	:
			XSetDashes(
				dpy, lineGC, 0, 
				dashdotlist, sizeof(dashdotlist)
			);
			break;

		case L_DASH_DOT_DOT:
			XSetDashes(
				dpy, lineGC, 0, 
				dashddlist, sizeof(dashddlist)
			);
			break;

		default :
			ESprintf(
				EINVAL, "Illegal or unsupported line type(%d)", 
				linetype
			);
			return(-1);
			break;
		}

		gcv.line_style = LineOnOffDash;
	}

	/* change line GC to reflect changes	*/
	XChangeGC(dpy, lineGC, GCLineStyle, &gcv);

	return(0);
}


/*	GCsetlinewidth
 *
 *		set the GC line attribute. LINE_WIDTH_MODE must be
 *	scaled else no action is taken
 *
 *	on entry
 *		linewidth	: is the desired line attribute
 *	on exit
 *		lineGC	: has line attribute linewidth
 */
		
int	lineWidthScale = 1;	/* line width scaling factor	*/
static	int	GCsetlinewidth(linewidth)
	Rtype	linewidth;
{
	unsigned long	mask;
	int	ilinewidth;

	if (LINE_WIDTH_MODE != MODE_SCALED) {
		ESprintf(ENOSYS,"Unsupported scaling mode(%d)",LINE_WIDTH_MODE);
		return(-1);
	}

	/*
	 * scale the line. A hack for super high resolution pixmaps
	 */
	linewidth *= lineWidthScale;
	linewidth = linewidth < 0.0 ? 0.0 : linewidth;

	ilinewidth = (int) ROUND(linewidth);
	/* 
	 * if line width is 1 then set it to the X11 line with of 
	 * 0. A line width of 0 in X is actually a line width of one
	 * pixel but is drawn with a faster algorithm. 
	 * If linewidth is 0 than no line is to be drawn. The simplest 
	 * way to implement this is to set the src/dest pixel function to
	 * `noop'
	 */
	if (ilinewidth == 0) {	/* draw nothing	*/
		gcv.function = GXnoop;
		mask = GCFunction;

	} else if (ilinewidth == 1) {
		gcv.line_width = 0;
		gcv.join_style = JoinMiter;
		gcv.function = GXcopy;
		mask = GCLineWidth | GCJoinStyle | GCFunction;
	} 
	else {
		gcv.join_style = JoinRound;

		/*
		 * for fat lines change the join style to round instead of
	 	 * miter
		 */
		gcv.line_width = ilinewidth;
		gcv.function = GXcopy;
		mask = GCLineWidth | GCJoinStyle | GCFunction;
	}


	/* change line GC to reflect changes	*/
	XChangeGC(dpy, lineGC, mask, &gcv);

	return (0);
}
/*
 *	encode_pixels
 *	[internal]
 *
 *	encode the color palette into a form that is easier to access
 *	with PUT_PIX()
 *
 * on entry
 *	*src		: list of pixels
 *	n		: len of src
 *	pixel_size	: size of a single pixel
 *	byte_order	: byte order to encode for (LSBFirst | MSBFirst)
 * on exit
 *	*dst		: the encoded pixels
 */
static	void	encode_pixels(src, dst, n, pixel_size, byte_order)
	Pixeltype	*src, *dst;
	unsigned	n, 
			pixel_size;
	int		byte_order;
{

	unsigned long	swaptest = 1;
	unsigned short	swap = FALSE;
	unsigned char	*left, *right, c;

	int		i, j;


	/*
	 * find out if we're on a byte swapped (LSBFirst) machine
	 */
	if (((*(char *) &swaptest) && (byte_order != LSBFirst))
		|| (!(*(char *) &swaptest) && (byte_order == LSBFirst))) {

		swap = TRUE;
	}


	/*
	 * encode the pixel table
	 */
	for (i=0; i<n; i++) {

		dst[i] = src[i];

		/*
		 * swap byte if needed
		 */
		if (swap) {
			left = (unsigned char *) &dst[i];
			right = left + sizeof (dst[i]) - 1;
			while (left < right) {
				c = *left;
				*left++ = *right;
				*right-- = c;
			}
		}

		/*
		 * left shift data so first significant byte is the 
		 * first byte (only need to do this if byte order is 
		 * MSBFirst, else its already done)
		 */
		if (byte_order == MSBFirst) {
			left = (unsigned char *) &dst[i];
			right = left + sizeof (dst[i]) - pixel_size;
			for (j=0; j<pixel_size; j++) {
				*left++ = *right++;
			}
		}
	}
}
static	int	sim_polygon(xp_list,n)
	XPoint	*xp_list;
	unsigned	n;

{
	int	i,j;

	FillTable	*fill_table;


	Ptype *p_list = (Ptype *) malloc (n * sizeof(Ptype));

	if (! p_list) {
		ESprintf(errno, "malloc(%d)", n * sizeof(Ptype));
		return(-1);
	}

	for (i = 0; i < n; i++) {
		p_list[i].x = xp_list[i].x;
		p_list[i].y = xp_list[i].y;
	}

	fill_table = buildFillTable(p_list, (unsigned) n);

	for (i = fill_table->y_first; i < (fill_table->y_last + 1); i++)
	{
		for ( j = 0; j < (fill_table->x_count[XC_INDEX(i)] - 1); j+=2) {

			XDrawLine(dpy, drawable, polygonGC,
				(short) fill_table->x_coord[XC_INDEX(i)][j],
				(short) i,
				(short) fill_table->x_coord[XC_INDEX(i)][j+1],
				(short) i);
		}
	}

	free ((Voidptr) p_list);
	return(1);
}


/*ARGSUSED*/
static	int	x11_non_rect_cell_array(c, color_pal, P, Q, R, nx, ny)
	CGMC		*c;
        Pixeltype       *color_pal;
        Ptype		P, Q, R;
        int		nx, ny;
{
        return(0);      /* non rectangular cell arrays are not supported */
}



/*
 * macro for copying a pixel from a pixel table into a character array
 */
#define	PUT_PIX(pal, pal_ind, dst, size)	\
	{					\
	int	i;				\
	char	*s, *d;				\
	s = (char *) &(pal)[(pal_ind)];		\
	d = (char *) dst;			\
	for (i=0; i<size; i++) {		\
		*d++ = *s++;			\
	}					\
	}
		
/*
 *	x11_cell_array
 *	[internal]
 *
 *	render a rectangular cell array
 *
 * on entry
 *	*color_pal	: array of X pixels
 *	P,Q,R		: corners of the cell array (See CGM standard)
 *	nx		: number of cells in x direction
 *	ny		: number of cells in y direction
 * on exit
 *	return		: 0 => Ok, else error
 */
static	int	x11_cell_array(c, color_pal, P, Q, R, nx, ny)
	CGMC		*c;
	Pixeltype	*color_pal;
	Ptype	P, Q, R;
	int	nx, ny;
{
	unsigned int	image_height,	/* image height in pixels	*/
			image_width,	/* image width in pixels	*/
			image_size,	/* size of image data in bytes	*/
			pad;		/* number of bytes padding	*/
	unsigned	pixel_size;	/* size of a single pixel	*/
	char		*data,		/* image data			*/
			*cptr;

	int		step_x,		/* step size for incrementing in
					 * x direction within the image
					 */
			step_y;		/* step size for incrementing in
					 * y direction within the image
					 */

	int		start_x, 
			start_y;	/* destination of image in drawable */

	int		*rows, 
			*cols;		/* information about the number of
					 * pixels making up a row (col) in
					 * a the cell at row (col)[i]
					 */
	int		*index_array,	/* color indeces for a cell row	*/
			index;		/* color index for current cell */
	int		cgmc_index;	/* index into the cgmc		*/

	Pixeltype	pixels[MAX_COLOR_SIZE];
	XImage		*ximage;	/* the X image			*/

	register int	i,j,k,l;

	image_width = ABS(P.x - Q.x) + 1;
	image_height = ABS(P.y - Q.y) + 1;

	/*
	 * don't know how to handle a cell array with zero dimension
	 */
	if (nx == 0 || ny == 0) return (0);

	rows = (int *) malloc ((unsigned) ny * sizeof (int));
	if (! rows) {
		ESprintf(errno, "malloc(%d)", ny* sizeof(int));
		return(-1);
	}

	cols = (int *) malloc ((unsigned) nx * sizeof (int));
	if (! cols) {
		ESprintf(errno, "malloc(%d)", nx * sizeof(int));
		return(-1);
	}

	index_array = (int *) malloc ((unsigned) nx * sizeof (int));
	if (! index_array) {
		ESprintf(errno, "malloc(%d)", nx * sizeof(int));
		return(-1);
	}

	ximage = XCreateImage(dpy, bestVisual, DspDepth, ZPixmap, 0, NULL,
		image_width, image_height, 32, 0);


	image_size = ximage->bytes_per_line * image_height;
	ximage->data = (char *) malloc(image_size);
	if (! ximage->data) {
		ESprintf(errno, "malloc(%d)", image_size);
		return(-1);
	}

	data = ximage->data;


	if (ximage->bits_per_pixel % 8) {
		ESprintf(
			E_UNKNOWN,
			"Cell arrays not supported on monochrome devices"
		);
		return(-1);	/* pixel size must be byte multible	*/
	}

	pixel_size = ximage->bits_per_pixel / 8;
	pad = ximage->bytes_per_line - pixel_size*image_width;

 
	/*
	 * encode the color palette into a form that is easier to access
	 * with PUT_PIX()
	 */
	encode_pixels(color_pal, pixels, MAX_COLOR_SIZE, pixel_size, 
			ximage->byte_order);

	/*
	 * calculate x & y steping size, position of image in the window,
	 * and starting address for data destination
	 */
	SetUpCellArrayAddressing(P, Q, R, image_size, pad, pixel_size, 
			(unsigned) ximage->bytes_per_line, 0,0, &step_x,&step_y,
			&start_x, &start_y, &data);

	/*
	 * set up rows and cols arrays with info about number of pixels
	 * making up each cell. We do this to avoid floating point arithmatic
	 * later on
	 */
	SetUpCellArrayIndexing(image_width, image_height, rows, cols, 
						(unsigned) nx, (unsigned) ny);


	/*
	 * process the rows
	 */
	cgmc_index = 0;
	for (i=0; i<ny; i++) {

		/* 
		 * load array of color indecies for row[i] of cells
		 */
                for (k=0; k<nx; k++) {

			/* make sure data available in cgmc     */
			if (cgmc_index >= c->Cnum && c->more) {
				if (Instr_Dec(c) < 1) {
					return (-1);
				}
				cgmc_index = 0;
			}
                        index_array[k] = c->c[cgmc_index];
			cgmc_index++;
                }

                /*      
		 * the rows of pixels per cell[i]
		 */
                for (j=0; j < rows[i]; j++) {

			cptr = data;
			/*
			 * the coloumns
			 */
			for (k=0; k<nx; k++) {


				/*
				 * the coloums of pixels per cell
				 */
				index = index_array[k];
				for (l=0; l<cols[k]; l++) {
					PUT_PIX(pixels,index, cptr,pixel_size);
					cptr += step_x;
				}
			
			}
			data += step_y;	/* skip to next row	*/
		}
	}

	/*
	 * copy image to the window
	 */
	XPutImage(dpy, drawable, cellGC, ximage, 0, 0, start_x, start_y,
					image_width, image_height);


	XDestroyImage(ximage);	/* frees ximage->data too	*/
	free((Voidptr) rows);
	free((Voidptr) cols);
	free((Voidptr) index_array);

	return(0);
}





/*ARGSUSED*/
int	X11_PolyLine(c)
CGMC *c;
{

/* Polyline routine.  Assume lineGC attributes
 * have been appropriately set.  Draw polylines in batches of a maximum of
 * points.  
 */

	int	status = 0;

	register int	n;	/* count of processed polyline coordinates */
	register int	p;	/* count processed unsent line coordinates */

	static	boolean	MoreData = FALSE;
	static	XPoint	P;
	int	x,y;


	/*
	 *	make sure line attributes are set
	 */
	if (Color_ava && COLOUR_TABLE_DAMAGE) {
		if (X11_UpdateColorTable_() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (Color_ava && LINE_COLOUR_DAMAGE) {
		if (GCsetcolor(LINE_COLOUR, lineGC) < 0) status = -1;
		LINE_COLOUR_DAMAGE = FALSE;
	}
		

	if (LINE_WIDTH_DAMAGE) {
		(void) GCsetlinewidth(LINE_WIDTH);
		LINE_WIDTH_DAMAGE = FALSE;
	}

	if (LINE_TYPE_DAMAGE) {
		(void) GCsetlinetype(LINE_TYPE);
		LINE_TYPE_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		GCsetclipping();
		CLIP_DAMAGE = FALSE;
	}

	/*
	 * This is a hack to ensure that something gets drawn in the
	 * case that all the points are the same. XDrawLines will NOT
	 * draw anything if all points in vertices list are identical
	 */
	if (((x = XConvert(c->p[0].x)) == XConvert(c->p[1].x)) 
		  && ((y = YConvert(c->p[0].y)) == YConvert(c->p[1].y))) {
	
		XDrawPoint(dpy, drawable, lineGC, x, y);
	}

	n = p = 0;

	/*
	 * deal with partitioned CGM point data. A polyline may be defined
	 * by multible sequential CGMCs
	 *
	 */
	if (MoreData) {	/* first point is last point from last call	*/
		Points.P[p++] = P;
		MoreData = FALSE;	/* maybe	*/
	}

	/* Draw lines in groups of (Points.size-1), except for the 
	 * last group. n = count of processed points .
	 * p = count of processed unsent point specifications.
	 */

	while (n < c->Pnum)
	{

		Points.P[p].x = (short) XConvert(c->p[n].x);
		Points.P[p].y = (short) YConvert(c->p[n].y);
		n++;
		if (++p == Points.size)
		{
			--p;
			XDrawLines(dpy, drawable, 
					lineGC, Points.P, 
					Points.size, CoordModeOrigin);

			Points.P[0].x = Points.P[p].x;
			Points.P[0].y = Points.P[p].y;
			p = 1;
		}
	}
	if (p > 1)
		XDrawLines(dpy, drawable, lineGC, Points.P,p,CoordModeOrigin);

	if (c->more) {	/* save last point if data is partitioned	*/
		n--;
		P.x = (short) XConvert(c->p[n].x);
		P.y = (short) YConvert(c->p[n].y);
		MoreData = TRUE;
	}

	return (status);

}

/*ARGSUSED*/
int	X11_DisjtLine(c)
	CGMC *c;
{
	/* 
	 * Disjoint Polyline routine.  Assume lineGC attributes
	 * have been appropriately set.  Draw segments in batches of a 
	 * maximum of points.  
	 */

	int	status = 0;

	register int	i;	/* count of processed disj line coordinates */
	register int	p;	/* count processed unsent line coordinates */

	XSegment	xsegments[POINTS_ALLOCED];


	/*
	 *	make sure line attributes are set
	 */
	if (Color_ava && COLOUR_TABLE_DAMAGE) {
		if (X11_UpdateColorTable_() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (Color_ava && LINE_COLOUR_DAMAGE) {
		if (GCsetcolor(LINE_COLOUR, lineGC) < 0) status = -1;
		LINE_COLOUR_DAMAGE = FALSE;
	}
		

	if (LINE_WIDTH_DAMAGE) {
		(void) GCsetlinewidth(LINE_WIDTH);
		LINE_WIDTH_DAMAGE = FALSE;
	}

	if (LINE_TYPE_DAMAGE) {
		(void) GCsetlinetype(LINE_TYPE);
		LINE_TYPE_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		GCsetclipping();
		CLIP_DAMAGE = FALSE;
	}

	if (ODD(c->Pnum)) c->Pnum--;	/* must be even	*/

	/* Draw lines in groups of sizeof(xsegments)  except for the 
	 * last group. n = count of processed points .
	 * p = count of processed unsent point specifications.
	 */
	for(i=0,p=0; i<c->Pnum; i+=2) 
	{
		xsegments[p].x1 = (short) XConvert(c->p[i].x);
		xsegments[p].y1 = (short) YConvert(c->p[i].y);
		xsegments[p].x2 = (short) XConvert(c->p[i+1].x);
		xsegments[p].y2 = (short) YConvert(c->p[i+1].y);

		if (++p == sizeof(xsegments))
		{
			XDrawSegments(
				dpy, drawable, lineGC, xsegments, 
				sizeof(xsegments)
				);

			p = 0;
		}
	}
	if (p > 0) XDrawSegments(dpy, drawable, lineGC, xsegments, p);

	return (status);

}

int	X11_PolyMarker(c)
	CGMC *c;
{

	int	offset;
	int	i;
	int	status = 0;

	/*
	 *	make sure marker attributes are set
	 */
	if (Color_ava && COLOUR_TABLE_DAMAGE) {
		if (X11_UpdateColorTable_() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (Color_ava && MARKER_COLOUR_DAMAGE) {
		if (GCsetcolor(MARKER_COLOUR, markerGC) < 0) status = -1;
		MARKER_COLOUR_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		GCsetclipping();
		CLIP_DAMAGE = FALSE;
	}
	offset = MARKER_SIZE / 2;

	switch (MARKER_TYPE) {
	case MARKER_X:	
		for(i=0;i<c->Pnum;i++) {
			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y - offset ),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y + offset ));

			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y + offset),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y - offset ));
		}
		break;
	case MARKER_CIRCLE:
		for(i=0;i<c->Pnum;i++) {
			XDrawArc(dpy, drawable, markerGC, 
				(int) (XConvert(c->p[i].x) - XScale(offset)),
				(int) (YConvert(c->p[i].y) - YScale(offset)),
				(int) XScale(offset * 2),
				(int) YScale(offset * 2),
				(360 * 64), (360 * 64));
		}
		break;
	case MARKER_STAR:
		for(i=0;i<c->Pnum;i++) {
			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y));

			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y + offset ),
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y - offset ));

			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y - offset ),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y + offset ));

			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y + offset ),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y - offset ));
		}
		break;
	case MARKER_PLUS:
		for(i=0;i<c->Pnum;i++) {
			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y));

			XDrawLine(dpy, drawable, markerGC,  
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y + offset ),
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y - offset ));
		}
		break;
	case MARKER_DOT:
		for(i=0;i<c->Pnum;i++) 
			XDrawPoint(dpy, drawable, markerGC, 
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y));
		break;
	default:
		/* unsupported polymarker type	*/
		ESprintf(
			EINVAL, "Illegal or unsupported marker type(%d)", 
			MARKER_TYPE
		);
		return (-1);
	}

	return (status);
}


/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_RestrText(c)
CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_ApndText(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	X11_Polygon(c)
	CGMC *c;
{

/* Polygon routine.  Assume polygonGC attributes
 * have been appropriately set.  
 */

/*	requested dimensions for fill stipple. See section 5.4.4	*/
#define	PWIDTH		8
#define	PHEIGHT		8



	register int	i; 	/* count of processed polygon coordinates */
	long	num_points = 0;	/* number of points to process		*/
	long	xindex = 0;	/* index into the point list		*/
	int	status = 0;


	/*
	 *	edge attributes not supported
	 */

	/*
	 *	make sure polygon attributes are set
	 */
	if (Color_ava && COLOUR_TABLE_DAMAGE) {
		if (X11_UpdateColorTable_() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (Color_ava && FILL_COLOUR_DAMAGE) { 
		if (GCsetcolor(FILL_COLOUR, polygonGC) < 0) status = -1;
		FILL_COLOUR_DAMAGE = FALSE;
	}

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		GCsetclipping();
		CLIP_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
	 * get the points from the cgmc
	 */
	while (1) {
		num_points += c->Pnum;
		/*	
		 *	make sure point buffer is big enough since we 
		 *	cannot easily break up the polygon like we can a 
		 *	polyline.
		 */
		if (Points.size < num_points) {

			Points.P = (XPoint *) realloc (
				(char *) Points.P, (unsigned) num_points 
				* sizeof(XPoint)
			);
			if (! Points.P) {
				ESprintf(
					errno, "realloc(%d)",
					num_points * sizeof(XPoint)
				);
				return(-1);
			}


			Points.size = num_points;
		}

		/*
		 *	convert VDC to X coordinates
		 */
		for (i=0; i < c->Pnum; i++, xindex++) {
			Points.P[xindex].x = (short) XConvert(c->p[i].x);
			Points.P[xindex].y = (short) YConvert(c->p[i].y);
		}

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
	 *	switch on interior style of fill area
	 */
	switch (INT_STYLE) {
		case	HOLLOW_S:

			/*	make sure first and last point are same	*/
			if (Points.P[num_points-1].x != Points.P[0].x 
				|| Points.P[num_points-1].y != Points.P[0].y) {

				Points.P[num_points].x = Points.P[0].x;
				Points.P[num_points].y = Points.P[0].y;
				num_points++;
			}

			/*	just draw a polyline	*/
			XDrawLines(dpy, drawable, polygonGC, Points.P, 
				(int) num_points, CoordModeOrigin);

			break;

		case	SOLID_S :
			if (! (*softFill)) {
				XFillPolygon(dpy, drawable, polygonGC, 
					Points.P, (int) num_points, Complex, 
					CoordModeOrigin);
			}
			/*
			 * if user wants software filling of polygons do it
			 */
			else {
				if (sim_polygon(
					Points.P,(unsigned)num_points)<0) {

					status = -1;
				}
			}

			break;

		case	PATTERN_S:

			/*
			 *	code to invoke a pattern routine
			 */
				/*fill patterns not supported	*/
			ESprintf(
				EINVAL,"Illegal or unsupported fill style(%d)", 
				INT_STYLE
			);
			status = -1;
			break;

		case	HATCH_S:

			/*	
			 *	Hatch indecies are simulated using a fill
			 *	stipple for the area. See section 5.4.3.
			 */
		

	/*	
	 *		create pixmap for stipple	
	 *	This needs to be done for each invocation of
	 *	the polygon routine because once a stipple is
	 *	set in a GC, X does not allow it to be changed
	 */

			if(stipple.stippleid = XCreatePixmap(dpy,drawable,
					stipple.width,stipple.height,1)){
				if(!stipple.gc){
					stipple.gc = XCreateGC(dpy,
						stipple.stippleid,0,NULL);
					if(!stipple.gc)
						goto GCFAILED;
				}

				stipple.P[1].x =stipple.P[2].x =stipple.width;
				stipple.P[2].y =stipple.P[3].y =stipple.height;


				/*	clear the stipple		*/
				XSetFunction(dpy,stipple.gc,GXclear);
				XFillPolygon(dpy,stipple.stippleid,stipple.gc,
					stipple.P,4,Convex,CoordModeOrigin);
				/*
				 *	only "set" pixels in the bitmap
				 *	will be set to the polygonGC color.
				 */
				XSetFunction(dpy,stipple.gc,GXset);

				switch (HATCH_IND) {

					case HORIZONTAL:
					default:

						/* draw new pattern	*/
						XDrawLine(dpy,stipple.stippleid,
							stipple.gc, (int) 0,
							stipple.height-1,
							stipple.width,
							stipple.height-1);



						break;

					case VERTICAL  :

						XDrawLine(dpy,stipple.stippleid,
							stipple.gc,
							stipple.width-1,
							0, stipple.width-1,
							stipple.height);

					break;

					case POSITIVE :

						XDrawLine(dpy,stipple.stippleid,
							stipple.gc, 
							stipple.width-1,0,
							0,stipple.height-1);

					break;

					case NEGATIVE  :

						XDrawLine(dpy,stipple.stippleid,
							stipple.gc, 0,0,
							stipple.width,
							stipple.height);

					break;

					case HORIZ_VERT:

						XDrawLine(dpy,stipple.stippleid,
							stipple.gc, 0,
							stipple.height-1,
							stipple.width,
							stipple.height-1);

						XDrawLine(dpy,stipple.stippleid,
							stipple.gc, 
							stipple.width-1,0,
							stipple.width-1,
							stipple.height);

					break;

					case POS_NEG  :


						XDrawLine(dpy,stipple.stippleid,
							stipple.gc,
							stipple.width,0,
							0,stipple.height);

						XDrawLine(dpy,stipple.stippleid,
							stipple.gc,
							0,0,
							stipple.width,
							stipple.height);

					break;
				}

				/* set the GC stipple parameter to stipple */
				XSetStipple(dpy,polygonGC,stipple.stippleid);

				/* change the fill style to use the stipple */
				XSetFillStyle(dpy,polygonGC,FillStippled);

			}
GCFAILED:

			XFillPolygon(dpy,drawable,polygonGC,
				Points.P,(int)num_points,Complex,
				CoordModeOrigin);
			
			/* if a stipple was created	*/
			if (stipple.stippleid) {

				/* set fill style back to normal	*/
				XSetFillStyle(dpy,polygonGC,FillSolid);

				/* free the stipple	*/
				XFreePixmap(dpy,stipple.stippleid);
				stipple.stippleid = None;

			}

			break;

		case	EMPTY_S:

			/*
			 *	do nothing 
			 */
			break;

		default:
			ESprintf(
				EINVAL, "Illegal or unsupported fill style(%d)",
				INT_STYLE
			);
			status = -1;
			break;
	}


	return (status);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_PolygonSet(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int	X11_CellArray(c)
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
	int	status = 0;

	/*
	 *	check any control elements
	 */
	if (Color_ava && COLOUR_TABLE_DAMAGE) {
		if (X11_UpdateColorTable_() < 0) status = -1;
		COLOUR_TABLE_DAMAGE = FALSE;
	}

	if (CLIP_DAMAGE) {
		GCsetclipping();
		CLIP_DAMAGE = FALSE;
	}

	startedDrawing = TRUE;

	/*
 	 *	extract data from cgmc
	 */

		/*	corners		*/
	P.x = XConvert(c->p[0].x);	P.y = YConvert(c->p[0].y);
	Q.x = XConvert(c->p[1].x);	Q.y = YConvert(c->p[1].y);
	R.x = XConvert(c->p[2].x);	R.y = YConvert(c->p[2].y);

		/*	dimensions	*/
	nx = c->i[0];		ny = c->i[1];


		/*	cell representation mode	*/
	mode = c->e[0];

	if (CSM != MODE_INDEXED) {
		ESprintf(EINVAL, "direct color not supported");
		return (-1);
	}

	if (mode != PACKED_MODE) {
		ESprintf(EINVAL, "run length encoding not supported");
		return(-1);
	}

        /*
         * see if cell array is rectangular or not
         */
        if (R.x != Q.x || P.y != R.y) {
                return (x11_non_rect_cell_array(c, Colortab,P,Q,R,nx,ny));
        }

        /*
         * cell array is a rectangluar
         */
        if (x11_cell_array(c, Colortab, P, Q, R, nx, ny) < 0) status = -1;

	return(status);
}


/*ARGSUSED*/
int	X11_GDP(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_Rect(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_Circle(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_Arc3Pt(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_Arc3PtClose(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_ArcCtr(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */ 
int	X11_ArcCtrClose(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_Ellipse(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_EllipArc(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_EllipArcClose(c)
	CGMC *c;
{
	startedDrawing = TRUE;

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}



/*	init_polygon:
 *
 *		initialize X11_Polygon routine. 
 *
 *	on exit
 *		Points.P : has POINTS_ALLOCED mem alloced
 */
					
int	init_polygon()
{
	int	status	= 0;
	/*	query best size of stipple. See section 5.4.4 */	
	if (!(XQueryBestStipple(dpy, drawable, PWIDTH, PHEIGHT,
		(unsigned int *)&stipple.width,
		(unsigned int *) &stipple.height))) {

		ESprintf(errno, "XQueryBestTile(,,,,,)");
		status = -1;
	}

	/*	allocate memory for the point buffer	*/

	if (!Points.size) { 
		Points.P = (XPoint *) malloc (
			(unsigned) (POINTS_ALLOCED * sizeof(XPoint))
		);
		if (! Points.P) {
			ESprintf(errno, "malloc()");
			return(-1);
		}

	}

	Points.size = POINTS_ALLOCED;

	return (status);
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

	radius /= 2;
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
		XDrawPoint(dpy, drawable, markerGC, x_out + xc, y + yc);
		XDrawPoint(dpy, drawable, markerGC, x_out + xc, -y + yc);
		XDrawPoint(dpy, drawable, markerGC, -x_out + xc, -y + yc);
		XDrawPoint(dpy, drawable, markerGC, -x_out + xc, y + yc);
	}
	for (y_out = y_start; y_out < y_end; ++y_out) {
		XDrawPoint(dpy, drawable, markerGC, y_out + xc, x + yc);
		XDrawPoint(dpy, drawable, markerGC, y_out + xc, -x + yc);
		XDrawPoint(dpy, drawable, markerGC, -y_out + xc, -x + yc);
		XDrawPoint(dpy, drawable, markerGC, -y_out + xc, x + yc);
	}
}
	
#define	SEGMENTS	64
static	quick_circle(xc,yc,radius)
	int	xc, yc;
	int	radius;
{
	int		x1,y1;
	register int	x2, y2;

	int		i;
	double		inc = (2.0 * (double) M_PI) / (double) SEGMENTS;	
	register double	theta = 0.0;

	x2 = ((int) ((float) radius * cos(theta))) + xc;
	y2 = ((int) ((float) radius * sin(theta))) + yc;
	for (i = 0; i < SEGMENTS; i++) {
		theta += inc;
		x1 = x2;
		y1 = y2;
		x2 = ((int) ((float) radius * cos(theta))) + xc;
		y2 = ((int) ((float) radius * sin(theta))) + yc;;
		XDrawLine(dpy, drawable, markerGC, x1, y1, x2, y2);
	}
}
#endif



