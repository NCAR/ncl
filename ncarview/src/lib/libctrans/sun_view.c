/*
 *	$Id: sun_view.c,v 1.6 1991-07-18 16:25:39 clyne Exp $
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
 *	sun_view.c:
 *
 *	Author		John Clyne
 *	Date		Tue Dec 12 12:30:58 MST 1989
 *
 *	This file contains the complete sunview driver for ctrans. 
 */
/*LINTLIBRARY*/
#include <stdio.h>
#include <pixrect/pixrect_hs.h>

#include	<cterror.h>
#include	<math.h>
#include	<ncarv.h>
#include	"cgmc.h"
#include	"default.h"
#include	"ctrandef.h"
#include	"sun_view.h"
#include	"soft_fill.h"
#include	"translate.h"

extern	char	*strcpy();
extern	char	*strcat();
extern	char	*strncpy();
extern	char	*getenv();
extern	boolean	stand_Alone;
extern	boolean	deviceIsInit;
extern	boolean	Batch;
extern	char	*program_name;
extern	boolean	*softFill;
extern	boolean	*bellOff;

/*
 *	command line options supported by sunview driver
 */
static	struct  {
        StringType_     Ws;	/* size of window		*/
        StringType_     Wp;	/* position of window		*/
        } commLineOpt;

static  Option  options[] =  {
        {"Ws", StringType, (unsigned long) &commLineOpt.Ws, 
						sizeof (StringType_ )},
        {"Wp", StringType, (unsigned long) &commLineOpt.Wp, 
						sizeof (StringType_ )},
        {NULL},
        };

#define	DEFAULT_WIDTH	800
#define	DEFAULT_HEIGHT	800

static	struct {		/* current info about the device*/	
	int	height,
		width,
		depth;
	} dev = {0,0,0}; 		

static	CoordModifier	dev_coord_mod = {0,0,1.0,1.0};

static	Canvas	canvas;		/* the drawing canvas			*/
static	Pixwin	*pixReg;	/* pixrect associated with canvas	*/
static	Pixwin	*pixWin;
static	Pixrect	*tile;

static	boolean	nextFrame = FALSE;
static	boolean	exposeEvent = FALSE;
static	boolean	colorAva = FALSE;	/* true if device has color	*/
static	boolean	firstFrame = TRUE; 	/* true if processing first frame*/



static	Frame	frame;

static	void	set_clipping();

/*
 *	The class 0 CGM element functions
 */
/*ARGSUSED*/
Ct_err	SunV_BegMF(c)
CGMC *c;
{

	void	input_event();
	void	expose_event();
	static	Notify_value	destroy_proc_();

	int	x,y;


	/*
	 * if device is already been initialized then don't bring up a
	 * new window or reallocate any memory
	 */
	if (!deviceIsInit) {

                /*
                 *      parse sunview specific command line args
                 *      (currently only geometry accepted       )
                 */
                getOptions((caddr_t) 0, options);

		/*
		 * convert string representing width and heigth to 
		 * width and heigth
		 */
		if (sscanf(commLineOpt.Ws,"%d %d",&dev.width,&dev.height) !=2){
			ct_error(T_NULL, "dimensions must be quoted");
			return(DIE);
		}

		if (dev.width == -1) dev.width = DEFAULT_WIDTH;
		if (dev.height == -1) dev.height = DEFAULT_HEIGHT;

		/*
		 * convert string representing position to 
		 * position
		 */
		if (sscanf(commLineOpt.Wp, "%d %d", &x, &y) != 2) {

			ct_error(T_NSO, "position must be quoted");
			return(DIE);
		}

		frame = window_create((Window) NULL, FRAME, 
				FRAME_LABEL,	program_name,
				FRAME_NO_CONFIRM,	TRUE,
				WIN_WIDTH,	dev.width,
				WIN_WIDTH,	dev.height,
				WIN_X,		x,
				WIN_Y,		y,
				WIN_EVENT_PROC,	expose_event,
				0);
		canvas = window_create(frame, CANVAS, 0);

		pixWin = canvas_pixwin(canvas);

		/*
		 * intercept requests from the user for frame destruction
		 */
		(void) notify_interpose_destroy_func(frame, destroy_proc_);

#ifdef	DEAD

		/*
		 * we draw in a region so we can do clipping
		 */
		pixReg = pw_region(pixWin, 0,0,1,1);
#else
		pixReg = pixWin;
#endif

		window_set(frame, WIN_SHOW, TRUE, 0);

		/*
		 * see if on a color display
		 */
		dev.depth = pixReg->pw_pixrect->pr_depth;
		if (dev.depth > 1) {
			colorAva = TRUE;
		}

		/*
		 * do some more onetime initilization
		 */
		init_sunv(colorAva);

	}

	/*
	 * wait for window to open before proceeding
	 */
	while (!exposeEvent) {
		(void) notify_dispatch();
	}


	/*
	 * now pass control on to the End Picture routine
	 */
	window_set(canvas, WIN_CONSUME_KBD_EVENT, WIN_ASCII_EVENTS,
				WIN_IGNORE_PICK_EVENT, WIN_NO_EVENTS,
				WIN_CONSUME_PICK_EVENT, MS_LEFT,
				WIN_EVENT_PROC,	input_event,
				0);

	deviceIsInit = TRUE;	/* we are initialized	*/
	return (OK);
}

/*ARGSUSED*/
Ct_err	SunV_EndMF(c)
CGMC *c;
{

	if (!deviceIsInit)
		return(OK);

	return (OK);
}

/*ARGSUSED*/
Ct_err	SunV_BegPic(c)
CGMC *c;
{

	int	i;
	int	width, 
		height;	/* width and height of window	*/

	CoordRect	dev_extent;

	/*
	 *	copy default table to working default table	
	 *	most of the CGM elements contain output attribute or
	 *	input processing information. This data is stored in a 
	 *	table in "default.h". SetInPic keeps the data up to date
	 *	for each new frame
	 */
	SetInPic((boolean) TRUE);

	/*
	 * assign color table pointers to point to current tables and
	 * not the old tables
	 */
	if (firstFrame) {
		color_tab.index = &color_tab.current_i[0];
		color_tab.rgb = &color_tab.current_rgb[0];
		color_tab.next_new_index = &color_tab.current_index;
	}
		

	/*
	 * restore color map to default value (whatever that may be)
	 */
	color_tab.current_index = color_tab.default_index;
	for (i = 0; i < color_tab.current_index; i++) {
		color_tab.current_i[i] = color_tab.default_i[i];
		color_tab.current_rgb[i] = color_tab.default_rgb[i];
		pw_putcolormap(pixReg, i, 1, &(color_tab.current_rgb[i].red),
				&(color_tab.current_rgb[i].green),
				&(color_tab.current_rgb[i].blue));
	}
		
		

	/*
	 *	find out how big window is. if its changed size recalculate
	 *	coordinate translation macros
	 */
	width = (int) window_get(canvas, CANVAS_WIDTH);
	height = (int) window_get(canvas, CANVAS_HEIGHT);

	if (dev.height != height || dev.width != width) {

		dev.height = height;
		dev.width = width;


		/*
		 * (re)initialize the software fill module. This needs to 
		 * be initialized every time the window changes sizes
		 */
		initSoftSim(dev.height, dev.width);


		/*
		 *	calculate X device coordinate transfer macro
		 *	leave room for title bar
		 */


		/*
		 *	calculate X device coordinate transfer macro
		 *	leave room for title bar
		 */
		dev_extent.llx = dev_extent.ury = 0;
		dev_extent.lly = dev.height - 1;
		dev_extent.urx = dev.width - 1;

		transinit(&dev_extent, dev_coord_mod, TRUE);

		/*
		 * set our clipping region to the size of the canvas
		 * (turn off clipping)
		 */
#ifdef	DEAD
		rect.r_left = rect.r_top = 0;
		rect.r_width = dev.width;
		rect.r_height = dev.height;
		pw_set_region_rect(pixReg, &rect, 1); 
#endif
	}


	return(OK);
}

/*ARGSUSED*/
Ct_err	SunV_BegPicBody(c)
CGMC *c;
{
	unsigned op = 0;

	/*	
	 *	check to see if any CGM picture descriptor elements 
	 *	were changed/received
	 */


#ifdef	BATCH
	/*
	 * turn on batching. This makes thinks draw faster under sunview
	 */
	pw_batch_on(pixReg);
#endif

	/* clear the screen	*/
	op = PIX_SRC | PIX_COLOR(0);
	pw_writebackground(pixReg, 0,0, dev.width, dev.height, op);
		

	return (OK);
}

/*ARGSUSED*/
Ct_err	SunV_EndPic(c)
CGMC *c;
{
#ifdef	BATCH
	/*
	 * turn off batching. 
	 */
	pw_batch_off(pixReg);
#endif


	/*
	 *	clear default table
	 */
	(void) SetInPic((boolean) FALSE);

	firstFrame = FALSE;

	/*
	 *	if not interactive don't perform any user interaction
	 */
	if (Batch) {
		return(OK);
	}

	/*
	 * if not stand alone do not interact with user. Let interface do it
	 */
	if (!stand_Alone) {
		return(OK);
	}

	/* 
	 *	Beep & wait for the user to press mouse button 1,
	 *	space bar, return or quit.
	 */
	if (! *bellOff) window_bell(frame);

	while(!nextFrame) {
		(void) notify_dispatch();
	}
	nextFrame = FALSE;	/* inform notifier to go on	*/
	return (OK);

}





/*
 *	The class 4 CGM elements functions
 */


/* Polyline routine.  Draw polylines in batches of a maximum of
 * points.  
 */
/*ARGSUSED*/
Ct_err	SunV_PolyLine(c)
CGMC *c;
{

	register int	n;	/* count of processed polyline coordinates */
	register int	p;	/* count processed unsent line coordinates */

	static	Pr_texture *tex = NULL;		/* line style		*/
	static	Pr_brush lineWidth = {1};
	unsigned	op = 0;

	Pr_texture	*set_line_type();


	op = PIX_SRC | PIX_COLOR(color_tab.index[LINE_COLOUR.index]);


	/*
	 * test and set line attributes
	 */
	if (LINE_WIDTH_DAMAGE) {
		lineWidth.width = (int) LINE_WIDTH;
		LINE_WIDTH_DAMAGE = FALSE;
	}

	if (LINE_TYPE_DAMAGE) {
		tex = set_line_type();
		LINE_TYPE_DAMAGE = FALSE;
	}
	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		set_clipping();
		CLIP_DAMAGE = FALSE;
	}

	n = p = 0;

		/* Draw lines in groups of (pointBuf.size-1), except for the 
		 * last group. n = count of processed points .
		 * p = count of processed unsent point specifications.
		 */
		while (n < c->Pnum)
		{
			pointBuf.p[p].x = (int) XConvert(c->p[n].x);
			pointBuf.p[p].y = (int) YConvert(c->p[n].y);
			n++;
			if (++p == POINT_BUF_ALLOCED)
			{
				--p;
				pw_polyline(pixReg,0,0, (int)POINT_BUF_ALLOCED, 
						pointBuf.p, POLY_DONTCLOSE, 
						(struct pr_brush *) &lineWidth, 
						(struct pr_texture *) tex, op);

				pointBuf.p[0].x = pointBuf.p[p].x;
				pointBuf.p[0].y = pointBuf.p[p].y;
				p = 1;
			}
		}

		/*
		 * if the line is of zero width than draw nothing
		 */
		if (LINE_WIDTH == 0.0) {
			op = PIX_DST | 
				PIX_COLOR(color_tab.index[LINE_COLOUR.index]);
		}

		if (p > 1)
			pw_polyline(pixReg, 0,0, p, 
					pointBuf.p, POLY_DONTCLOSE, 
					&lineWidth, tex, op);


	return (OK);

}

Ct_err	SunV_PolyMarker(c)
CGMC *c;
{

	int	offset;
	int	i;
	unsigned	op = PIX_SRC;

	op |= PIX_COLOR(color_tab.index[MARKER_COLOUR.index]);

	/*
	 *	make sure marker attributes are set
	 */


	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		set_clipping();
		CLIP_DAMAGE = FALSE;
	}

	offset = MARKER_SIZE / 2;

	switch (MARKER_TYPE) {
	case MARKER_X:	
		for(i=0;i<c->Pnum;i++) {
			pw_vector(pixReg,
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y - offset ),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y + offset ),
				op, 0);

			pw_vector(pixReg,
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y + offset),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y - offset ),
				op, 0);
		}
		break;
	case MARKER_CIRCLE:
		for(i=0;i<c->Pnum;i++) {
			quick_circle( (int) XConvert(c->p[i].x),
				(int) YConvert(c->p[i].y),
				(int) XScale(offset),
				op);
		}
		break;
	case MARKER_STAR:
		for(i=0;i<c->Pnum;i++) {
			pw_vector(pixReg,
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y),
				op, 0);

			pw_vector(pixReg,
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y + offset ),
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y - offset ),
				op, 0);

			pw_vector(pixReg,
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y - offset ),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y + offset ),
				op, 0);

			pw_vector(pixReg,
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y + offset ),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y - offset ),
				op, 0);
		}
		break;
	case MARKER_PLUS:
		for(i=0;i<c->Pnum;i++) {
			pw_vector(pixReg,
				(int) XConvert(c->p[i].x - offset), 
				(int) YConvert(c->p[i].y),
				(int) XConvert(c->p[i].x + offset), 
				(int) YConvert(c->p[i].y),
				op, 0);

			pw_vector(pixReg,
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y + offset ),
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y - offset ),
				op, 0);
		}
		break;
	case MARKER_DOT:
		for(i=0;i<c->Pnum;i++) 
			pw_vector(pixReg,
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y),
				(int) XConvert(c->p[i].x), 
				(int) YConvert(c->p[i].y),
				op, 0);
		break;
	default:
		/* unsupported polymarker type	*/
		ct_error(NT_UPMT,"");
		return (SICK);
	}


	return (OK);
}


/*ARGSUSED*/
/* 
 *	Polygon routine.  
 */
Ct_err	SunV_Polygon(c)
CGMC *c;
{

	register int	i; 	/* count of processed polygon coordinates */
	long	num_points = 0;	/* number of points to process		*/
	long	xindex = 0;	/* index into the point list		*/

	unsigned	op = PIX_SRC;

	extern	Ct_err	Instr_Dec();
	extern	char	*realloc();
	extern	char	*realloc();

	op |= PIX_COLOR(color_tab.index[FILL_COLOUR.index]);

	/*
	 *	make sure polygon attributes are set
	 */

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		set_clipping();
		CLIP_DAMAGE = FALSE;
	}

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
		if (pointBuf.size < num_points) {

			if ((pointBuf.p = (struct pr_pos *) 
				realloc ((char *) pointBuf.p, 
				(unsigned) num_points 
				* sizeof(struct pr_pos))) == NULL ) {

				ct_error(NT_MALLOC, "for poly points");
				return (SICK);
			}

			pointBuf.size = num_points;
		}

		/*
		 *	convert VDC to sun view coordinates
		 */
		for (i=0; i < c->Pnum; i++, xindex++) {
			pointBuf.p[xindex].x = (int) XConvert(c->p[i].x);
			pointBuf.p[xindex].y = (int) YConvert(c->p[i].y);
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
	 * if user wants software filling of polygons do it
	 */
	if (num_points > MAX_POLYGON_POINTS || *softFill) {
		sim_polygon(pointBuf.p, (int) num_points, op);
		return(OK);
	}


	/*
	 *	switch on interior style of fill area
	 */
	switch (INT_STYLE) {
	case	HOLLOW_S:

		/*	make sure first and last point are same	*/
		if (pointBuf.p[num_points-1].x != pointBuf.p[0].x 
			|| pointBuf.p[num_points-1].y != pointBuf.p[0].y) {

			pointBuf.p[num_points].x = pointBuf.p[0].x;
			pointBuf.p[num_points].y = pointBuf.p[0].y;
			i++;
		}

		/*	just draw a polyline	*/
		pw_polyline (pixReg, 0, 0, (int) num_points, pointBuf.p,
			POLY_DONTCLOSE, (struct pr_brush *) NULL, 
			(struct pr_texture *) NULL, op);

		break;

	case	SOLID_S :

		pw_polygon_2(pixReg, 0, 0, 1, &num_points, pointBuf.p,
			op, (Pixrect *) NULL, 0, 0);

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
		 * clear the tile for pattern replication
		 */
		pr_rop(tile, 0, 0, TILE_SIZE, TILE_SIZE, 
			PIX_SRC | PIX_COLOR(0), (Pixrect *) NULL, 0, 0);

		/*
		 * create the appropriate pattern in the tile for replication
		 */
		switch (HATCH_IND) {

		case	HORIZONTAL:

			/* draw new pattern     */
			pr_vector(tile, 0, TILE_SIZE - 1, TILE_SIZE - 1, 
				TILE_SIZE - 1, op, 0);
			break;

		case	VERTICAL:
			pr_vector(tile, TILE_SIZE - 1, 0, TILE_SIZE - 1, 
				TILE_SIZE - 1, op, 0);
			break;

		case	POSSITIVE:
			pr_vector(tile, 0, TILE_SIZE - 1, TILE_SIZE - 1, 
				0, op, 0);
			break;
			
		case	NEGATIVE:
			pr_vector(tile, 0, 0, TILE_SIZE - 1, 
				TILE_SIZE - 1, op, 0);
			break;

		case	HORIZ_VERT:
			pr_vector(tile, 0, TILE_SIZE - 1, TILE_SIZE - 1, 
				TILE_SIZE - 1, op, 0);
			pr_vector(tile, TILE_SIZE - 1, 0, TILE_SIZE - 1, 
				TILE_SIZE - 1, op, 0);
			break;

		case	POSS_NEG:
			pr_vector(tile, 0, TILE_SIZE - 1, TILE_SIZE - 1, 
				0, op, 0);
			pr_vector(tile, 0, 0, TILE_SIZE - 1, 
				TILE_SIZE - 1, op, 0);
			break;
		}

		
		pw_polygon_2 (pixReg, 0, 0, 1, &num_points, pointBuf.p,
			op, tile, 0, 0);
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
Ct_err	SunV_CellArray(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"SunV_CellArray\n");
#endif DEBUG

#define	PACKED_MODE	1


	/* points giving boundry of cell array	*/
	Ptype	P,	/* LOWER left corner (See above)	*/
		Q,	/* upper right corner			*/
		R;	/* lower right				*/	

	Itype	nx, ny;		/* dimensions of cell array by number of cells	*/
	Etype	mode;		/* cell representation mode		*/

	Ct_err	raster_();

	/*
	 *	check any control elements
	 */
	if (CLIP_DAMAGE) {
		set_clipping();
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

		SetUpCellArrayIndexing(ABS(P.x - Q.x) + 1, ABS(P.y - Q.y) + 1,
			rows, cols, (unsigned) nx, (unsigned) ny);
		
		/*
		 * how is cell array stored
		 */
		if (mode == PACKED_MODE) {
			/*
			 *	use raster instructions
			 */
			P.y = Q.y;
			(void) raster_(c, P, rows, cols, 
				(int) nx, (int) ny, abs((int) (P.x - R.x)),
				abs((int) (Q.y - R.y)));
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
		ct_error(NT_CAFE, "cell array must be a rectangle");
		return (SICK);
	}

	return (OK);
}



/*
 *	sim_polygon
 *	[internal]
 *
 *	simulate a filled polygon with vectors.  buildFillTable must have
 *	been called previously with the correct device information.
 * on entry:
 *	sun_pt_list	: list of verticies
 *	n		: number of verticies
 *	op		: the fill color
 */
static	sim_polygon(sun_pt_list, n, op)
	struct	pr_pos *sun_pt_list;
	int	n;
	unsigned	op;

{
	int	i,j;

	FillTable	*fill_table;

	extern	FillTable	*buildFillTable();

	Ptype *p_list = (Ptype *) icMalloc ((unsigned) (n * sizeof(Ptype)));

	for (i = 0; i < n; i++) {
		p_list[i].x = sun_pt_list[i].x;
		p_list[i].y = sun_pt_list[i].y;
	}

	fill_table = buildFillTable(p_list, (unsigned) n);

	for (i = fill_table->y_first; i < (fill_table->y_last + 1); i++)
	{
		for ( j = 0; j < (fill_table->x_count[i] - 1); j+=2) {

			pw_line(pixReg, (int) fill_table->x_coord[i][j],
					(int) i,
					(int) fill_table->x_coord[i][j+1],
					(int) i, NULL, NULL, op);
		}
	}

	cfree ((char *) p_list);
}

/*
 *	Class 5 elements
 */
/*ARGSUSED*/
Ct_err	SunV_ColrTable(c)
CGMC *c;
{

	int	i,
		index;

	Rgb	rgb;
	char	msg[80];

	/* see if device supports colour	*/
	if (!colorAva)
		return (OK);		/* punt!	*/


	for (index=c->ci[0], i = 0 ;index< (c->ci[0] + c->CDnum); index++,i++) {

		rgb.red = c->cd[i].red;
		rgb.green = c->cd[i].green;
		rgb.blue = c->cd[i].blue;

		/*
		 * if we have a new index try and add it to color table
		 * if the index is not new we just overwrite its value
		 */
		if (color_tab.index[index] == UNALLOCATED) {

			/* make sure there is room	*/
			if (*color_tab.next_new_index < MAX_COLOR)  {
				color_tab.index[index] = 
					*color_tab.next_new_index;

				(*color_tab.next_new_index)++;
			}
			else {
				(void)sprintf(msg, ": only %d colors available",
					MAX_COLOR);
				ct_error(NT_CAE, msg);
				return(SICK);
			}
		}
		/*
		 * record the color
		 */
		color_tab.rgb[color_tab.index[index]] = rgb;

		/*
		 * store the color in the color map
		 */
		pw_putcolormap(pixReg, color_tab.index[index], 
			1, &rgb.red, &rgb.green, &rgb.blue);
	}
	return (OK);
}

#define	SEGMENTS	64
static	quick_circle(xc, yc, radius, op)
	int     xc, yc;
	int     radius;
	unsigned	op;
{
	extern  double  cos();
	extern  double  sin();

	int             x1,y1;
	register int    x2, y2;

	int             i;
	double          inc = (2.0 * (double) M_PI) / (double) SEGMENTS;
	register double theta = 0.0;

	x2 = ((int) ((float) radius * cos(theta))) + xc;
	y2 = ((int) ((float) radius * sin(theta))) + yc;

		for (i = 0; i < SEGMENTS; i++) {
		theta += inc;
		x1 = x2;
		y1 = y2;
		x2 = ((int) ((float) radius * cos(theta))) + xc;
		y2 = ((int) ((float) radius * sin(theta))) + yc;;
		pw_vector(pixReg, x1, y1, x2, y2, op, 0);
	}
}


/*	raster_
 *
 *		simulate a cell array using raster instructions that have 
 * 	a "packed" encoding. See discussion on Cell arrays in 
 *	"NCAR Graphics installation guide"
 *
 * on entry:
 *	c		: the CGMC containing the data
 *	P		: the 'P' coordinate of a rectangular cell array
 *	*rows		: a list of the number of pixels in each cell in a row
 *	*cols		: a list of the number of pixels in each cell in a cols
 *	nx		: number of colums in cell array
 *	ny		: number of rows in cell array
 *	width		: width of the cell array in pixels.
 *	height		: height of the cell array in pixels.
 */
static	Ct_err	raster_(c, P, rows, cols, nx, ny, width, height)

	CGMC    *c;
	Ptype  P;
	int     *rows, *cols;
	int     nx, ny;
	int     width,
		height;

{

	register	int k,l;
	register	int i,j;

	unsigned	*index_array = NULL;  /* single row of cell pixel vals*/

	int	bytes_per_line;

	int	index = 0;      /* index for color list in cgmc */

	Pixrect	*pw;	/* the memory image of the cell array	*/

	char	*md_image,
		*ptr;		/* pointer to pixrect data	*/

	unsigned char mask;


	/*
	 * alloc memory for in memory pixrect and index_array
	 */
	if ((pw = mem_create(width, height, dev.depth)) ==
		NULL)  {
		ct_error(NT_MALLOC, "cell array too big");
		return (SICK);
	}
	index_array = (unsigned *) icMalloc ((unsigned) nx * sizeof(unsigned));

	/*
	 * find out how many bytes are in a row of pixels. Rows are
	 * aligned on 32 bit boundries
	 */
	bytes_per_line = width * dev.depth;
	if (bytes_per_line & 31) {	/* need to pad	*/
		bytes_per_line += ((~bytes_per_line) & 31) + 1;
	}
	bytes_per_line = bytes_per_line >> 3;


	md_image = (char *) mpr_d(pw)->md_image;

	/*
	 * cell arrays are encoded from bottom left to top right. pixrects
	 * are encoded top left to bottom right
	 */
	md_image += ((height - 1) * bytes_per_line);


	/*	the rows	*/
	for (i=0; i < ny; i++ ) {

		/* load  array of color indecies for that row of cells	*/
		for (k=0; k < nx; k++, index++) {

			/* make sure data available in cgmc     */
			if (index == c->Cnum && c->more) {
				if (Instr_Dec(c) != OK)
					return (pre_err);

				index = 0;
			}

			index_array[k] = c->c[index];
		}
		
		/*	the rows of pixels per cell	*/
		for (j=0; j < rows[i]; j++) {



			/*	the coloumns	*/
			ptr = md_image;
			mask = (char) 128;
			*ptr = 0;
			for (k=0; k<nx; k++) {


				/*	the coloums of pixels per cell	*/
				for (l=0; l < cols[k]; l++) {
					if (dev.depth != 1) {
						*ptr = (short) index_array[k];
						ptr++;
					}
					else {
						*ptr |= (index_array[k] ? 
							mask : 0);
						mask = mask >> 1;
						if (!mask) {
							*(++ptr) = 0;
							mask = (char) 128;
						}
					}
				}
			}
			md_image -= bytes_per_line;

		}

	
	}

	/*
	 * render the pixrect to the screen
	 */
	pw_rop(pixReg, (int) P.x, (int) P.y, width, height, PIX_SRC, pw, 0, 0);

	if (index_array != (unsigned *) NULL) 
		cfree((char *) index_array);

	pr_destroy(pw);

	return (OK);
}

static	void	set_clipping()
{
	Rect	rect;
	int	x1, y1, x2, y2;

	if (!(CLIPFLAG)) {
		/*
		 * turn clipping off
		 */
		rect.r_left = rect.r_top = 0;
		rect.r_width = dev.width;
		rect.r_height = dev.height;

	}
	else {	/* turn it on at the desired location	*/

		/*      else; turn on clipping  */

		x1 = XConvert(CLIPXMIN);
		x2 = XConvert(CLIPXMAX);
		y1 = YConvert(CLIPYMIN);
		y2 = YConvert(CLIPYMAX);
 

		/* make sure the pair (x1, y1) is the upper left corner */
		if (x1 > x2) {
			rect.r_width = x1 - x2 + 1;
			x1 = x2;
		}
		else
			rect.r_width = x2 - x1 + 1;

		if (y1 > y2) {
			rect.r_height = y1 - y2 + 1;
			y1 = y2;
		}
		else
			rect.r_height = y2 - y1 + 1;

		rect.r_left = x1;
		rect.r_top = y1;
	}


#ifdef	DEAD
	pw_close(pixReg);
	pixReg = pw_region(pixWin, x1, y1, rect.r_width, rect.r_height);

	pw_set_region_rect(pixReg, &rect, 1);
#endif
}


/*	set_line_type
 *
 */
static	Pr_texture	*set_line_type()
{
	static	Pr_texture	pr_texture;

	if (LINE_TYPE == L_SOLID)
		return((Pr_texture *) NULL);

	else {
		switch(LINE_TYPE) {
		case L_DASH	:

			pr_texture.pattern = pr_tex_dotted;
			break;

		case L_DOT	:

			pr_texture.pattern = pr_tex_dashed;
			break;

		case L_DASH_DOT	:

			pr_texture.pattern = pr_tex_dashdot;
			break;

		case L_DASH_DOT_DOT:

			pr_texture.pattern = pr_tex_dashdotdotted;
			break;

		default :
			ct_error(NT_UPLS,"");
			break;
		}
	}

	return(&pr_texture);

}

/*ARGSUSED*/
static	Notify_value destroy_proc_(frame, status)
	Frame	frame;
	Destroy_status	status;
{
	if (status == DESTROY_CHECKING) {
		return(NOTIFY_DONE);
	}

	close_ctrans();
	exit(0);
}

/*
 *	init_sunv
 *	[internal]
 *	
 *	set up the color map and allocate memory for ctrans
 * on entry
 *	color_ava	: true if device has colour
 */
static	init_sunv(color_ava)
	boolean	color_ava;
{
	int	i;
	unsigned char	red[MAX_COLOR],
			green[MAX_COLOR],
			blue[MAX_COLOR];

	pointBuf.p = (struct pr_pos *) icMalloc 
		(POINT_BUF_ALLOCED * sizeof (struct pr_pos));

	pointBuf.size = POINT_BUF_ALLOCED;

	if (color_ava) {
		/*
		 * init the color stuff
		 */
		for (i = 0; i < MAX_COLOR_INDEX; i++) {
			color_tab.default_i[i] = UNALLOCATED;
			color_tab.current_i[i] = UNALLOCATED;
		}

		/*
		 * get the default foreground and background from the 
		 * default colormap
		 */
		pw_getcolormap(pixReg, 0, INITIAL_COLOR_ALLOCATION, 
							red, green, blue);

		color_tab.default_index = 
		color_tab.current_index = INITIAL_COLOR_ALLOCATION;

		color_tab.next_new_index = &color_tab.default_index;
		color_tab.index = &color_tab.default_i[0];
		color_tab.rgb = &color_tab.default_rgb[0];

		/*
		 * record the foreground and background color in the 
		 * color_tab. These values can be overwritten by the CGM
		 */
		color_tab.index[0] = 0;
		color_tab.index[1] = 1;

		color_tab.rgb[0].red = red[0];
		color_tab.rgb[0].green = green[0];
		color_tab.rgb[0].blue = blue[0];
		color_tab.rgb[1].red = red[1];
		color_tab.rgb[1].green = green[1];
		color_tab.rgb[1].blue = blue[1];

		
		/*
		 * claim our own portion of the colormap with the default
		 * foreground and background colors
		 */
		pw_setcmsname(pixReg, "ctrans");
		pw_putcolormap(pixReg, 0, MAX_COLOR, red, green, blue);
	}

	/*
	 * create a pixrect for polygon hatch pattern replication
	 */
	tile = mem_create(TILE_SIZE, TILE_SIZE, dev.depth);
}


/*
 * 	event handlers
 */
/*ARGSUSED*/
void	expose_event(window, event, arg)
	Window	window;
	Event	*event;
	caddr_t	arg;
{
	exposeEvent = TRUE;
}
/*ARGSUSED*/
void	input_event(window, event, arg)
	Window	window;
	Event	*event;
	caddr_t	arg;
{

	switch event_action(event) {
	case	MS_LEFT	:
	case	'n'	:
	case	''	:
		nextFrame	= TRUE;
		break;

	case	'q'	:
		close_ctrans();
		exit(0);
	}

}
