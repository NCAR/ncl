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
#include	<math.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	<cgm_tools.h>
#include	<cterror.h>
#include	"cgmc.h"
#include	"default.h"
#include	"Xdefs.h"
#include	"ctrandef.h"
#include	"Xcrm.h"
#include	"soft_fill.h"
#include	"translate.h"

#ifdef	BSD
#define RINT(X)	rint((double) X)
#else
#define	RINT(X)	((int) X)	/* no round to an int routine in sys V	*/
#endif

extern  Ct_err	Instr_Dec();
extern	Ct_err	set_background_colr();

extern	Colormap	Cmap;

extern	boolean	Colordef[];
extern	boolean	*softFill;
extern	Pixeltype	Colortab[];
extern	boolean	Color_ava;

static	struct	{	/* a pixmap for tileing a filled polygon*/
	XPoint	P[4];
	Pixmap	tileid;
	int	width,
		height;
	} tile = { 0,0, 0,0, 0,0, 0,0, 
			0, 0, 0};	
	
/*	
 *	stuff needed to support line styles in X
 */
#define	DASH_LIST_LEN		2
#define	DOT_LIST_LEN		2
#define	DASHDOT_LIST_LEN	4	
#define	DASHDD_LIST_LEN		6	

#define	DASHSIZE		4
#define	DOTSIZE			1
#define	GAPSIZE			4

static	unsigned	char	
	dashlist[DASH_LIST_LEN] = {DASHSIZE,GAPSIZE};

static	unsigned	char	
	dotlist[DOT_LIST_LEN] = {DOTSIZE,GAPSIZE};

static	unsigned	char	
	dashdotlist[DASHDOT_LIST_LEN] = {DASHSIZE, GAPSIZE,DOTSIZE,GAPSIZE};

static	unsigned	char	
	dashddlist[DASHDD_LIST_LEN] = {DASHSIZE,GAPSIZE,DOTSIZE,
					GAPSIZE,DOTSIZE,GAPSIZE};

static	struct	{	/* list of line styles. See section 5.4.2	*/
	unsigned char	*dash;
	unsigned char	*dot;
	unsigned char	*dash_dot;
	unsigned char	*dash_dot_dot;
	} dashes = {
		dashlist,
		dotlist,
		dashdotlist,
		dashddlist,
	};		

/*
 * this flag is a hack to prevent bogus CGM's from attempting to change 
 * background colour after drawing has begun for a frame
 */
boolean startedDrawing = FALSE;

/*
 *	The class 4 CGM elements functions
 */


/*ARGSUSED*/
Ct_err	X11_PolyLine(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_PolyLine\n");
#endif

/* Polyline routine.  Assume lineGC attributes
 * have been appropriately set.  Draw polylines in batches of a maximum of
 * points.  
 */

	Ct_err	GCsetlinewidth();
	Ct_err	GCsetcolor();
	Ct_err	GCsetlinetype();

	register int	n;	/* count of processed polyline coordinates */
	register int	p;	/* count processed unsent line coordinates */

	static	boolean	MoreData = FALSE;
	static	XPoint	P;

	/*
	 *	make sure line attributes are set
	 */
	if (Color_ava && LINE_COLOUR_DAMAGE) {
		(void) GCsetcolor(LINE_COLOUR, lineGC);
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

	/*
	 * see if background color has changed
	 */
	if (BACKCOLR_DAMAGE ) {
		(void) set_background_colr(BACKCOLR);
		BACKCOLR_DAMAGE = FALSE;
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
	 *	a single pair of points with same coordinates is a point
	 */
	if ((c->Pnum == 2) && (c->p[0].x == c->p[1].x) 
		 	  && (c->p[0].y == c->p[1].y)) {
	
		XDrawPoint(dpy,drawable,lineGC,(int) XConvert(c->p[0].x),
				(int) YConvert(c->p[0].y));
		return (OK);
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

	return (OK);

}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_DisjtLine(c)
CGMC *c;
{
	startedDrawing = TRUE;
#ifdef DEBUG
	(void) fprintf(stderr,"X11_DisjtLine\n");
#endif DEBUG

	return (OK);
}

Ct_err	X11_PolyMarker(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_PolyMarker\n");
#endif DEBUG

	Ct_err	GCsetcolor();

	int	offset;
	int	i;

	/*
	 *	make sure marker attributes are set
	 */
	if (Color_ava && MARKER_COLOUR_DAMAGE) {
		(void) GCsetcolor(MARKER_COLOUR, markerGC);
		MARKER_COLOUR_DAMAGE = FALSE;
	}

	/*
	 * see if background color has changed
	 */
	if (BACKCOLR_DAMAGE ) {
		(void) set_background_colr(BACKCOLR);
		BACKCOLR_DAMAGE = FALSE;
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
		ct_error(NT_UPMT,"");
		return (SICK);
	}



	return (OK);
}


/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_RestrText(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_RestrText\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_ApndText(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_ApndText\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
Ct_err	X11_Polygon(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Polygon\n");
#endif DEBUG


/* Polygon routine.  Assume polygonGC attributes
 * have been appropriately set.  
 */

/*	requested dimensions for fill tile. See section 5.4.4	*/
#define	PWIDTH		8
#define	PHEIGHT		8


	Ct_err	GCsetcolor();

	register int	i; 	/* count of processed polygon coordinates */
	long	num_points = 0;	/* number of points to process		*/
	long	xindex = 0;	/* index into the point list		*/


	/*
	 *	edge attributes not supported
	 */

	/*
	 *	make sure polygon attributes are set
	 */
	if (Color_ava && FILL_COLOUR_DAMAGE) { 
		(void) GCsetcolor(FILL_COLOUR, polygonGC);
		FILL_COLOUR_DAMAGE = FALSE;
	}

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		GCsetclipping();
		CLIP_DAMAGE = FALSE;
	}

	/*
	 * see if background color has changed
	 */
	if (BACKCOLR_DAMAGE ) {
		(void) set_background_colr(BACKCOLR);
		BACKCOLR_DAMAGE = FALSE;
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

			Points.P = (XPoint *) icRealloc ((char *) Points.P, 
				(unsigned) num_points 
				* sizeof(XPoint));


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
			if (Instr_Dec(c) != OK) return (pre_err);
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
				sim_polygon(Points.P, (unsigned) num_points);
			}

			break;

		case	PATTERN_S:

			/*
			 *	code to invoke a pattern routine
			 */
				/*fill patterns not supported	*/
			ct_error(NT_UPFS, "pattern");
			break;

		case	HATCH_S:

			/*	
			 *	Hatch indecies are simulated using a fill tile
			 *	for the area. See section 5.4.3.
			 */
		

	/*	
	 *		create pixmap for tile	
	 *	This needs to be done for each invocation of
	 *	the polygon routine because once a tile is
	 *	set in a GC, X does not allow it to be changed
	 */

			if (tile.tileid = XCreatePixmap(dpy, drawable, 
				tile.width, 
				tile.height, 
				DefaultDepth(dpy,DefaultScreen(dpy)))) {

				tile.P[1].x = tile.P[2].x = tile.width;
				tile.P[2].y = tile.P[3].y = tile.height;


				/*	clear the tile		*/
				XFillPolygon(dpy,tile.tileid,tileGC,
					tile.P, 4, Complex,CoordModeOrigin);

				/*	need to turn off clipping to draw
				 *	in our tile with the polygonGC
				 */
				if (CLIPFLAG) {
					XSetClipMask(dpy, polygonGC, None);
				}

				switch (HATCH_IND) {

					case HORIZONTAL:
					default:

						/* draw new pattern	*/
						XDrawLine(dpy, tile.tileid, 
							polygonGC, (int) 0,
							tile.height-1,
							tile.width,
							tile.height-1);



						break;

					case VERTICAL  :

						XDrawLine(dpy, tile.tileid, 
							polygonGC, tile.width-1,
							0, tile.width-1,
							tile.height);

					break;

					case POSSITIVE :


						XDrawLine(dpy, tile.tileid, 
							polygonGC, 
							tile.width,0,
							0,tile.height);

					break;

					case NEGATIVE  :

						XDrawLine(dpy, tile.tileid, 
							polygonGC, 0,0,
							tile.width,tile.height);

					break;

					case HORIZ_VERT:

						XDrawLine(dpy, tile.tileid, 
							polygonGC, 0,
							tile.height-1,
							tile.width,
							tile.height-1);

						XDrawLine(dpy, tile.tileid, 
							polygonGC, 
							tile.width-1,0,
							tile.width-1,
							tile.height);

					break;

					case POSS_NEG  :


						XDrawLine(dpy, tile.tileid, 
							polygonGC, tile.width,0,
							0,tile.height);

						XDrawLine(dpy, tile.tileid, 
							polygonGC, 0,0,
							tile.width,tile.height);

					break;
				}

				/* set the GC tile parameter to our tile */
				XSetTile(dpy, polygonGC, tile.tileid);

				/* change the fill style to use the tile */
				XSetFillStyle(dpy, polygonGC, FillTiled);

			}

			XFillPolygon(dpy, drawable, polygonGC, 
				Points.P, (int) num_points, Complex, 
				CoordModeOrigin);
			
			/* if a tile was created	*/
			if (tile.tileid) {

				/* set fill style back to normal	*/
				XSetFillStyle(dpy, polygonGC, FillSolid);

				/* free the tile	*/
				XFreePixmap(dpy, tile.tileid);

				/* restore clipping if necessary	*/
				if (CLIPFLAG)
					GCsetclipping();
			}

			break;

		case	EMPTY_S:

			/*
			 *	do nothing 
			 */
			break;

		default:
			ct_error(NT_UPFS,"");
			return(SICK);
	}


	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_PolygonSet(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_PolygonSet\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
Ct_err	X11_CellArray(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_CellArray\n");
#endif DEBUG

#define	PACKED_MODE	1

	extern	Ct_err	raster_packed_cell_sim();	
	extern	Ct_err	polygon_packed_cell_sim();	


	/*	
	 *	programmers unfamiliar with CGM representation of Cell arrays
	 *	should see section 5.6.9  in the ANSI document on 
	 *	Computer Graphic Metafiles.
	 *
	 *	Note:
	 *		NCAR's CGM generator lables the lower left corner 
	 *	of the cell array P. The corner P should be the upper left 
	 *	corner of the cell array. This is a Bug in the generator.
	 */


	/* points giving boundry of cell array	*/
	Ptype	P,		/* LOWER left corner (See above)	*/
		Q,		/* upper right corner			*/
		R;		/* lower right				*/	

	Itype	nx, ny;		/* dimensions of cell array by number of cells*/
	Etype	mode;		/* cell representation mode		*/


	/*
	 * see if background color has changed
	 */
	if (BACKCOLR_DAMAGE ) {
		(void) set_background_colr(BACKCOLR);
		BACKCOLR_DAMAGE = FALSE;
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

	if (CSM != INDEXED) {
		ct_error(NT_CAFE, "direct color not supported");
		return (SICK);
	}

	/*
	 * see if cell array is rectangular for quick display with raster 
	 * instructions.
	 */
	if (P.y == R.y && R.x == Q.x) {
		int	*rows, *cols;

		cols = (int *) icMalloc((unsigned) nx * sizeof (int));
		rows = (int *) icMalloc((unsigned) ny * sizeof (int));

		cell_prep(P, Q, R, rows, cols, (unsigned) nx, (unsigned) ny);
		
		/*
		 * how is cell array stored
		 */
		if (mode == PACKED_MODE) {
			/*
			 *	use raster instructions
			 */
			(void) raster_packed_cell_sim(c, P, rows, cols, 
				(int) nx, (int) ny, abs((int) (P.x - R.x)));
		} 
		else {
			(void) fprintf(stderr, 
			"ctrans: run length encoded cell arrays not supported\n"
			);
		}
		if (rows) cfree((char *) rows);
		if (cols) cfree((char *) cols);
	}

	/* 
	 * cell array is NOT rectangular
	 */
	else {
		int	delta_pr_x,	/* length of x vector for delta_pr*/ 
			delta_pr_y;	/* length of y vector for delta_pr*/ 

		int	delta_qr_x,	/* length of x vector for delta_qr*/
			delta_qr_y;	/* length of y vector for delta_qr*/

		int	fudge_x,
			fudge_y;

		irregular_cell_prep (P, Q, R, (unsigned) nx, (unsigned) ny
			&delta_pr_x, &delta_pr_y,
			&delta_qr_x, &delta_qr_y,
			&fudge_x, &fudge_y);

		/*
		 * how is cell array stored
		 */
		if (mode == PACKED_MODE) {
			/*
			 *	draw individual cells using a fill polygon
			 */
			(void) polygon_packed_cell_sim(c,P,delta_pr_x, 
				delta_pr_y, delta_qr_x, 
				delta_qr_y, fudge_x, fudge_y, 
				(int) nx, (int) ny);
		}
		else {
			(void) fprintf(stderr, 
			"ctrans: run length encoded cell arrays not supported\n"
			);
		}
	}
	return (OK);
}


/*ARGSUSED*/
Ct_err	X11_GDP(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_GDP\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_Rect(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Rect\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_Circle(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Circle\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_Arc3Pt(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Arc3Pt\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_Arc3PtClose(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Arc3PtClose\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_ArcCtr(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_ArcCtr\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */ 
Ct_err	X11_ArcCtrClose(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_ArcCtrClose\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_Ellipse(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Ellipse\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_EllipArc(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_EllipArc\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_EllipArcClose(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_EllipArcClose\n");
#endif DEBUG
	startedDrawing = TRUE;

	return (OK);
}


/*	GCsetcolor:
 *
 *		set the color of a particular GC
 *
 *	on entry
 *		color	: the desired color
 *	on exit
 *		gc	: has foreround attribute set to color	
 */
Ct_err	GCsetcolor(color, gc)
	COtype	color;
	GC	gc;
{

	Pixeltype	pixel;
	char	buf[10];	/* error message buffer	*/

	/* check COLOUR SELECTION MODE	*/
	if (CSM == INDEXED) {

		/* see if the color has been defined	*/
		if (Colordef[color.index]) { 
			XSetForeground(dpy, gc, Colortab[color.index]);
		}
		else {
			/* set to default color if invalid index	*/
			XSetForeground(dpy, gc, Colortab[1]);

			(void) sprintf(buf, "%d", color.index);
			ct_error(NT_ICTI, buf);
		}
	}



	else {	/* colour selection mode is direct	*/

	}
	return (OK);
}


/*	GCsetlinetype
 *
 *		set the GC line attribute
 *	on entry
 *		linetype: is the desired line attribute
 *	on exit
 *		lineGC	: has line attribute linetype
 */
static	Ct_err	GCsetlinetype(linetype)
	IXtype	linetype;
{
	if (linetype == L_SOLID)
		gcv.line_style = LineSolid;

	else {
		switch(linetype) {
			case L_DASH	:
				XSetDashes(dpy, lineGC, 0, 
					dashes.dash, DASH_LIST_LEN);

				break;

			case L_DOT	:
				XSetDashes(dpy, lineGC, 0, 
					dashes.dot, DOT_LIST_LEN);

				break;

			case L_DASH_DOT	:
				XSetDashes(dpy, lineGC, 0, 
					dashes.dash_dot, DASHDOT_LIST_LEN);

				break;

			case L_DASH_DOT_DOT:
				XSetDashes(dpy, lineGC, 0, 
					dashes.dash_dot_dot, DASHDD_LIST_LEN);

				break;

			default :
				ct_error(NT_UPLS,"");
				break;
		}

		gcv.line_style = LineOnOffDash;
	}

	/* change line GC to reflect changes	*/
	XChangeGC(dpy, lineGC, GCLineStyle, &gcv);

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
static	Ct_err	GCsetlinewidth(linewidth)
	Rtype	linewidth;
{
	unsigned long	mask;

	if (LINE_WIDTH_MODE != SCALED) {
		ct_error(NT_ILSM, "absolute");
		return(SICK);
	}

	/*
	 * scale the line. A hack for super high resolution pixmaps
	 */
	linewidth *= lineWidthScale;
	/* 
	 * if line width is 1.0 then set it to the X11 line with of 
	 * 0. A line width of 0 in X is actually a line width of one
	 * pixel but is drawn with a faster algorithm. 
	 * If linewidth is 0 than no line is to be drawn. The simplest 
	 * way to implement this is to set the src/dest pixel function to
	 * `noop'
	 */
	if (linewidth == 0) {	/* draw nothing	*/
		gcv.function = GXnoop;
		mask = GCFunction;

	} else if (linewidth == 1.0) {
		gcv.line_width = 0;
		gcv.join_style = JoinMiter;
		mask = GCLineWidth | GCJoinStyle;
	} 
	else {
		gcv.join_style = JoinRound;

		/*
		 * for fat lines change the join style to round instead of
	 	 * miter
		 */
		gcv.line_width = RINT(linewidth);
		mask = GCLineWidth | GCJoinStyle;
	}


	/* change line GC to reflect changes	*/
	XChangeGC(dpy, lineGC, mask, &gcv);

	return (OK);
}

/*	init_polygon:
 *
 *		initialize X11_Polygon routine. 
 *
 *	on exit
 *		Points.P : has POINTS_ALLOCED mem alloced
 */
					
Ct_err	init_polygon()
{
	/*	query best size of tile. See section 5.4.4 */	
	if (!(XQueryBestTile(dpy, drawable, PWIDTH, PHEIGHT,
		&tile.width, &tile.height)))

		ct_error(NT_MALLOC,"");

	/*	allocate memory for the point buffer	*/

	if (!Points.size) { 
		Points.P = (XPoint *) 
			icMalloc ( POINTS_ALLOCED * (unsigned) sizeof(XPoint));

	}

	Points.size = POINTS_ALLOCED;

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
	extern	double	cos();
	extern	double	sin();

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



static	sim_polygon(xp_list,n)
	XPoint	*xp_list;
	unsigned	n;

{
	int	i,j;

	FillTable	*fill_table;

	extern	FillTable	*buildFillTable();

	Ptype *p_list = (Ptype *) icMalloc (n * sizeof(Ptype));

	for (i = 0; i < n; i++) {
		p_list[i].x = xp_list[i].x;
		p_list[i].y = xp_list[i].y;
	}

	fill_table = buildFillTable(p_list, (unsigned) n);

	for (i = fill_table->y_first; i < (fill_table->y_last + 1); i++)
	{
		for ( j = 0; j < (fill_table->x_count[i] - 1); j+=2) {

			XDrawLine(dpy, drawable, polygonGC,
					(short) fill_table->x_coord[i][j],
					(short) i,
					(short) fill_table->x_coord[i][j+1],
					(short) i);
		}
	}

	cfree ((char *) p_list);
}
