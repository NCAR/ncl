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
 *	cgi.c	ctrans driver for Sun's CGI library. This release
 *		does not support pixwins in that it does not use
 *		the Pixwin version of CGI.  This means that,
 *		while metafiles are translated to a window, the
 *		window does not have auto-refresh capability.
 *
 *		Don Middleton - NCAR - Summer 1988
 *
 *	[modified] 1/25/89	John clyne (clyne@bierstadt.ucar.edu)
 *		
 *		Added metafile default capability and hatch patterns
 *		for interior style
 */


#include <stdio.h>
#include <cgidefs.h>
#include <suntool/sunview.h>
#include <fcntl.h>
#include <cterror.h>
#include "ctrandef.h"
#include <math.h>
#include "cgmc.h"
#include "default.h"
#include "translate.h"

#ifndef lint
static char *RCSid = "$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libctrans/Attic/cgi.c,v 1.1 1990-12-11 13:32:53 clyne Exp $";
#endif

#define	MAXPOINT 1024 /* maximum number of coordinate points in a list */

extern	FILE	*tty;
extern	char	*malloc();
extern	char	*strcpy();

static Cint 	name;
static Cvwsurf 	device;
static char	*cmap_name;

static  CoordModifier   dev_coord_mod = {0,0,1.0,1.0};

extern	boolean	Batch;
extern	boolean	deviceIsInit;

static unsigned char r[256], g[256], b[256];
static Ccentry table = { r, g, b, 0 };


static Ccoor	VDCll = {0, 0};
static Ccoor	VDCur = {32767, 32767};

static	boolean	Colr_ava = FALSE ;

/*
 *	The default colormap used if user does not supply his own
 */
static unsigned char red[] =
{  0,255,255,  0,  0,  0,255,255,255,128,  0,  0,128,255, 85,170} ;
static unsigned char green[] =
{  0,255,  0,255,  0,255,  0,255,128,255,255,128,  0,  0, 85,170} ;
static unsigned char blue[] =
{  0,64,  0,  0,255,255,255,  0,  0,  0,128,255,255,128, 85,170} ;

static	Ccentry default_colors  = {red, green, blue, 16};

/* Class 0 */

/*ARGSUSED*/
Ct_err	CGIBegMF(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIBegMF\n");
#endif DEBUG

	Ct_err	CGIEndMF();
	Ct_err	color_device();

	if (deviceIsInit) 
		(void) CGIEndMF(c);

	open_cgi();

#ifdef DEAD
	/* Sun's retaining code does not work */
	device.retained = 1;
#endif DEAD


	
	/*
	 *	find out if we are on a color device
	 */
	(void) color_device(&Colr_ava);

	if (!Colr_ava) {
		/* 
		 *	a monochrome device 
		 */
		NORMAL_VWSURF(device, BWPIXWINDD);

		/*
		 *	open a view surface if monochrome. (See section 2.1)
		 *	SunCGI Reference Manual.
		 */
		open_vws(&name,&device);

	}
	else {

		/*
		 *	colour device (we think)
		 */
		NORMAL_VWSURF(device, CGPIXWINDD);

		device.cmapsize = 128;		/* color map size	*/

		(void) strcpy (device.cmapname, cmap_name);

		open_vws(&name, &device);

	}



	/*
	 *	set CGI defaults
	 */
	reset_to_defaults();

	/*
	 *	change defaults that don't agree with CGM specification
	 */
	marker_size((Cfloat) ((MARKER_SIZE / XMAX - XMIN) * 100));
	line_width_specification_mode(ABSOLUTE);

	/*
	 * cgi driver has been successfully initialized
	 */
	deviceIsInit = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIEndMF(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEndMF\n");
#endif DEBUG

	/*
	 *	CGIEndMF is called when the coresponding element appears
	 *	in the metafile AND under abnormal termination. So check
	 *	to see if CGI driver was actually initialized before attempting
	 *	to close it
	 */

	if (!deviceIsInit)
		return(OK);

	close_vws(name);
	close_cgi();

	deviceIsInit = FALSE;

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIBegPic(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIBegPic\n");
#endif DEBUG

	/* set VDC extent	*/
	VDCll.x = XMIN; VDCll.y = YMIN;
	VDCur.x = XMAX; VDCur.y = YMAX;
	vdc_extent(&VDCll, &VDCur);

	/*	the following are generally CGM standard defaults 
	 *	concerned with "text" attributes.
	 *	They will be reset at the beginning of each frame. Note;
	 *	this overides any default supplied by the user in the form
	 *	of a "Metafile Default Element". This is not the correct
	 *	way to implement this. However, it is not expected that many
	 *	users will want to supply a default other then the norm.
	 *	for text attributes
	 */

	character_expansion_factor( (Cfloat) 1.0 );
	character_height( (Cint) (0.01 * VDCur.x) );
	character_spacing( (Cfloat) 0.0 );
	text_precision( STROKE );

        /*
         *      copy default table to working default table
         *      most of the CGM elements contain output attribute or
         *      input processing information. This data is stored in a
         *      table in "default.h". SetInPic keeps the data up to date
         *      for each new frame
         */
        SetInPic((boolean) TRUE);



#ifdef DEAD
	interior_style(SOLIDI, OFF);
	set_drawing_mode(OPAQUE, BITTRUE, BITTRUE, REPLACE);
	set_global_drawing_mode(OPAQUE, BITTRUE, BITTRUE, REPLACE);
#endif DEAD

	if (Colr_ava)
		color_table( (Cint) 0, &default_colors);

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIBegPicBody(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIBegPicBody\n");
#endif DEBUG

	clear_view_surface(name, 0, 0);

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIEndPic(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEndPic\n");
#endif DEBUG


        /*
         *      clear default table
         */
        (void) SetInPic((boolean) FALSE);



	if (Batch)
		return(OK);

	/* wait until there is a Carriage Return */
	while (getc(tty) != '\n');	


	return (OK);
}
/* Class 1 */

/*ARGSUSED*/
Ct_err	CGIMFVersion(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIMFVersion\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIMFDesc(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIMFDesc\n");
#endif DEBUG

	return (OK);
}


/*ARGSUSED*/
Ct_err	CGIMFElemList(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIMFElemList\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIMFDefaults(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIMFDefaults\n");
#endif DEBUG

        /*
         *      this function is implemented by the code in X11_BegMF
         *      and the default table in "default.c"
         */

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGICharSetList(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGICharSetList\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGICharCoding(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGICharCoding\n");
#endif DEBUG

	return (OK);
}



/* Class 2 */


/*ARGSUSED*/
Ct_err	CGIBackColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIBackColr\n");
#endif DEBUG

	/* no support in SunCGI */

	return (OK);
}



/* Class 3 */


/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIAuxColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIAuxColr\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGITransparency(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGITransparency\n");
#endif DEBUG

	return (OK);
}


/* Class 4 */



/*ARGSUSED*/
Ct_err	CGIPolyLine(c)
CGMC *c;
{
	static	Ccoorlist	list;
	static	int		coordspace = 0;
	unsigned	numToMalloc;
	register int	p,n;

#ifdef DEBUG
	(void) fprintf(stderr,"CGIPolyLine\n");
#endif
	/*
	 *	test/set line attributes
	 */

	if (Colr_ava && LINE_COLOUR_DAMAGE && CSM == INDEXED) {
                (void) line_color((Cint) LINE_COLOUR.index); 
                LINE_COLOUR_DAMAGE = FALSE;
        }

	if (LINE_WIDTH_DAMAGE) {
                (void) line_width( (float) LINE_WIDTH);
                LINE_WIDTH_DAMAGE = FALSE;
        }

        if (LINE_TYPE_DAMAGE) {
                (void) setlinetype(LINE_TYPE);
                LINE_TYPE_DAMAGE = FALSE;
        }

	/*
         *      check any control elements
         */
        if (CLIP_DAMAGE) {
                setclipping();
                CLIP_DAMAGE = FALSE;
	}

	if (c->Pnum > coordspace) {
		numToMalloc = (unsigned) 
			((c->Pnum > MAXPOINT) ? MAXPOINT : c->Pnum);

		if (list.ptlist != (Ccoor *) NULL) cfree( (char *) list.ptlist);
		list.ptlist = (Ccoor *)
			malloc(numToMalloc * (unsigned)sizeof(Ccoor));

		coordspace = numToMalloc;
	}

        n = p = 0;

	/* Draw lines in groups of (coordspace - 1), except for the
	 * last group. n = count of processed points .
	 * p = count of processed unsent point specifications.
	 */
	while (n < c->Pnum)
	{
		list.ptlist[p].x = (Cint) (c->p[n].x);
		list.ptlist[p].y = (Cint) (c->p[n].y);
		n++;
		if (++p == coordspace)
		{
			--p;
			list.n = coordspace;
			polyline(&list);

			list.ptlist[0].x = list.ptlist[p].x;
			list.ptlist[0].y = list.ptlist[p].y;
			p = 1;
		}
	}

	if (p > 1) {
		list.n = p;
		polyline(&list);
	}
 

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIDisjtLine(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIDisjtLine\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIPolyMarker(c)
CGMC *c;
{
	static	Ccoorlist	list;
	static	int		coordspace = 0;
	int	i;
	unsigned	numToMalloc;

#ifdef DEBUG
	(void) fprintf(stderr,"CGIPolyMarker\n");
#endif DEBUG


        /*
         *      make sure marker attributes are set
         */
        if (Colr_ava && MARKER_COLOUR_DAMAGE && CSM == INDEXED) {
                (void) marker_color((Cint) MARKER_COLOUR.index);
                MARKER_COLOUR_DAMAGE = FALSE;
        }

        if (MARKER_TYPE_DAMAGE ) {
                (void) marker_type((Cint) MARKER_TYPE - 1);
                MARKER_TYPE_DAMAGE = FALSE;
        }

        if (MARKER_SIZE_DAMAGE ) {
		marker_size((Cfloat) ((MARKER_SIZE / XMAX - XMIN) * 100));
                MARKER_SIZE_DAMAGE = FALSE;
        }

	/*
         *      check any control elements
         */
        if (CLIP_DAMAGE) {
                setclipping();
                CLIP_DAMAGE = FALSE;
        }

	if (c->Pnum > coordspace) {
		numToMalloc = (unsigned)
			((c->Pnum > MAXPOINT) ? MAXPOINT : c->Pnum);

		if (list.ptlist != (Ccoor *) NULL) cfree( (char *) list.ptlist);
		list.ptlist = (Ccoor *)
			malloc(numToMalloc * (unsigned)sizeof(Ccoor));

		coordspace = numToMalloc;
	}

	for(i=0;i<c->Pnum;i++) {
		list.ptlist[i].x = (Cint)c->p[i].x;
		list.ptlist[i].y = (Cint)c->p[i].y;
	}

	list.n = c->Pnum;

	polymarker(&list);

	return (OK);
}


/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIRestrText(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIRestrText\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIApndText(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIApndText\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIPolygon(c)
CGMC *c;
{
	static	Ccoorlist	list;
	static	int		coordspace = 0;
	int	i;
	unsigned	numToMalloc;

#ifdef DEBUG
	(void) fprintf(stderr,"CGIPolygon\n");
#endif DEBUG





	/*
         *      make sure polygon attributes are set
         */
        if (Colr_ava && FILL_COLOUR_DAMAGE) {
                (void) fill_color((Cint) FILL_COLOUR.index);
                (void) perimeter_color((Cint) FILL_COLOUR.index);
                FILL_COLOUR_DAMAGE = FALSE;
        }

        if (INT_STYLE_DAMAGE) {
		setintstyle();
                INT_STYLE_DAMAGE = FALSE;
	}

        /*
         *      check any control elements
         */
        if (CLIP_DAMAGE) {
                setclipping();
                CLIP_DAMAGE = FALSE;
        }



	/* Sun's documentation says 255 is the max number of points.
	Empirical evidence indicates its more like 1K. */

	if (c->Pnum > MAXPOINT) {
		(void) printf("Polygon points > 1024 (%d)\n",c->Pnum);
		c->Pnum = MAXPOINT;
	}

	if (c->Pnum > coordspace) {
		numToMalloc = (unsigned) (c->Pnum);

		if (list.ptlist != (Ccoor *) NULL) cfree( (char *) list.ptlist);
		list.ptlist = (Ccoor *)
			malloc(numToMalloc * (unsigned)sizeof(Ccoor));

		coordspace = numToMalloc;
	}

	for(i=0;i<c->Pnum;i++) {
		list.ptlist[i].x = (Cint)c->p[i].x;
		list.ptlist[i].y = (Cint)c->p[i].y;
	}
	list.n = c->Pnum;

	polygon(&list);

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIPolygonSet(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIPolygonSet\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGICellArray(c)
CGMC *c;
{

#ifdef DEBUG
	(void) fprintf(stderr,"CGICellArray\n");
#endif DEBUG

#define	PACKED_MODE	1

	extern	Ct_err	cgi_packed_cell_sim();


	CoordRect       dev_extent;

	/*	
	 *	Users unfamiliar with CGM representation of Cell arrays
	 *	should see section 5.6.9  in the ANSI document on 
	 *	Computer Graphic Metafiles.
	 *
	 *	Note:
	 *		NCAR's CGM generator lables the lower left corner of
	 *		the cell array P. The corner P should be the upper left
	 *		corner of the cell array.This is a Bug in the 
	 *		generator.
	 */


	/* points giving boundry of cell array	*/
	Ccoor	P,		/* lower left corner (See below diagram)*/
		Q,		/* upper right corner			*/
		R;		/* lower right				*/	

	Itype	nx, ny;		/* dimension of cell array by number of cells*/
	Etype	mode;		/* cell representation mode		*/

	float	pr, qr;		/* length of lines pr and qr		*/

	float	delta_pr,	/* length of segment of a cell along line pr */
		delta_qr;	/* length of segment of a cell along line qr */

	Cint	delta_pr_x,	/* length of x vector for delta_pr	*/ 
		delta_pr_y,	/* length of y vector for delta_pr	*/ 

		delta_qr_x,	/* length of x vector for delta_qr	*/
		delta_qr_y;	/* length of y vector for delta_qr	*/



	int	fudge_x,
		fudge_y;	/* we draw one cell at a time. In order to
				 * to have the cells dimensions add up to 
				 * the boundries specified by P, Q and R
				 * cell may not be the same size.
				 */

	/*
	 *			diagram of values
	 *
	 *
	 *						    pr.
	 *		              _______________________________________Q
	 *		             /	              /
	 *		            /  	       	     /
	 *	 		   /	   	    /
	 *		     	  /   	     	   /
	 *	  delta_qr_x.    /	          /
	 *	         ______ /________________/__________
	 *         	|      /   delta_pr^    /
	 *	  	|     /		       /
	 *	  	|    /		      /
	 *  delta_qr_y> |   / < delta_qr     /
	 *	  	|  /		    /
	 *	  	| /	           /
	 *	  	|/________________/________________________________
	 *	  	P						   R
	 *					pr^
	 *		
	 *
	 */



	/*
	 *	find dimensions of window we're rendering in
	 */
	Cint	xbase, ybase,
		xext, yext;
	Cfloat	xunits, yunits;		/* not used	*/

	Ccoor	bot_left, top_right;	/* coordinates of window	*/

	inquire_physical_coordinate_system(name, &xbase, &ybase, &xext, &yext,
			&xunits, &yunits);

	/*
	 *	redefine VDC extent so it maps 1-1 to pixels
	 */
	bot_left.x = xbase; bot_left.y = ybase;
	top_right.x = xext; top_right.y = yext;
	vdc_extent(&bot_left, &top_right);

	/*
	 *	calculate conversion from CGM VDC to cgi defined VDC 
	 */
	dev_extent.llx = bot_left.x;
	dev_extent.lly = bot_left.y;
	dev_extent.urx = top_right.x;
	dev_extent.ury = top_right.y;

	transinit(&dev_extent, dev_coord_mod, TRUE);

	/*
	 *	set fill style
	 */
	interior_style(SOLIDI, OFF);

	
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


	/*	calculate lengths of cell array boundries	*/
	pr = (float) sqrt((double) (SQR(P.y - R.y) + SQR(P.x - R.x)));
	qr = (float) sqrt((double) (SQR(Q.y - R.y) + SQR(Q.x - R.x)));

	/*	calculate fudge factor			*/
	fudge_x = (int) pr % nx;
	fudge_y = (int) qr % ny;

	/*	calculate length of individual cell boundries	*/
	delta_pr = pr / nx;
	delta_qr = qr / ny;

	/*	calculate lengths of vectors describing a cell boundry	*/
	delta_pr_x  = (int) ((delta_pr * (R.x - P.x)) / pr);
	delta_pr_y  = (int) ((delta_pr * (R.y - P.y)) / pr);

	delta_qr_x  = (int) ((delta_qr * (Q.x - R.x)) / qr);
	delta_qr_y  = (int) ((delta_qr * (Q.y - R.y)) / qr);

#define	PACKED_MODE	1
	if (mode == PACKED_MODE) {
	/*
	 *	cell array uses packed encoding	
	 */

		(void) cgi_packed_cell_sim(c, P, delta_pr_x, delta_pr_y, 
			delta_qr_x, delta_qr_y, fudge_x, fudge_y, 
			(int) nx, (int) ny);

	}
	else {
		ct_error(NT_CAFE, "run length encoding not supported");
	}
		

	/*
	 *	restore old VDC extent
	 */
	bot_left.x = 0; bot_left.y = 0;
	top_right.x = 32767; top_right.y = 32767;
	vdc_extent(&bot_left, &top_right);

	/*
	 *	flag interior style damage
	 */
	INT_STYLE_DAMAGE = TRUE;

	return (OK);
}


/*ARGSUSED*/
Ct_err	CGIGDP(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIGDP\n");
#endif DEBUG

	/* no support in SunCGI */

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIRect(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIRect\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGICircle(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGICircle\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIArc3Pt(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIArc3Pt\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIArc3PtClose(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIArc3PtClose\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIArcCtr(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIArcCtr\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIArcCtrClose(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIArcCtrClose\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIEllipse(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEllipse\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIEllipArc(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEllipArc\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIEllipArcClose(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEllipArcClose\n");
#endif DEBUG

	return (OK);
}



/* Class 5 */


/*ARGSUSED*/
Ct_err	CGILineIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGILineIndex\n");
#endif DEBUG

	/* polyline_bundle_index( (Cint) c->ix[0] ); */

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIMarkerIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIMarkerIndex\n");
#endif DEBUG

	/* polymarker_bundle_index( (Cint) c->ix[0] ); */

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGITextIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGITextIndex\n");
#endif DEBUG

	text_bundle_index( (Cint) c->ix[0] );

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGICharSetIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGICharSetIndex\n");
#endif DEBUG

	/* SunCGI supports only set 1, which is the default */

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIAltCharSetIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIAltCharSetIndex\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIFillIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIFillIndex\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIPatIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIPatIndex\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIEdgeIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEdgeIndex\n");
#endif DEBUG

	return (OK);
}


/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIPatTable(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIPatTable\n");
#endif DEBUG

	return (OK);
}


/*ARGSUSED*/
Ct_err	CGIColrTable(c)
CGMC *c;
{
	Cint	start;
	int	i;

#ifdef DEBUG
	(void) fprintf(stderr,"CGIColrTable\n");
#endif DEBUG

	if (!Colr_ava)
		return (OK);

	start = (Cint) c->ci[0];

	for (i=0; i<c->CDnum; i++)
	{
		table.ra[i] =		c->cd[i].red;
		table.ga[i] =		c->cd[i].green;
		table.ba[i] =		c->cd[i].blue;
	}

	table.n = (int) c->CDnum;

	color_table(start, &table);

	return (OK);
}

/*ARGSUSED*/
Ct_err	CGIASF(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIASF\n");
#endif DEBUG

	return (OK);
}

/* Class 6 */
/*ARGSUSED*/
Ct_err	CGIEscape(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIEscape\n");
#endif DEBUG

	return (OK);
}


/* Class 7 */

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIMessage(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIMessage\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	CGIApplData(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"CGIApplData\n");
#endif DEBUG

	return (OK);
}

#ifdef DEAD
Cerror CGIInitColormap()
{



/* corresponds to tektronix 41xx defaults */
	static unsigned char red[] =
	{  0,255,255,  0,  0,  0,255,255,255,128,  0,  0,128,255, 85,170} ;
	static unsigned char green[] =
	{  0,255,  0,255,  0,255,  0,255,128,255,255,128,  0,  0, 85,170} ;
	static unsigned char blue[] =
	{  0,255,  0,  0,255,255,255,  0,  0,  0,128,255,255,128, 85,170} ;

/*	these numbers were inserted because Dan Anderson wanted to
	film something, even though the black (tek default) background
	colors were basically correct.  his camera did not like them.
	4/21/87 don middleton
*/

	static unsigned char red[] =
	{  255, 0, 255,  0,  0,  0,255,255,255,128,  0,  0,128,255, 85,170} ;
	static unsigned char greenn[] =
	{  255,0,  0,255,  0,255,  0,255,128,255,255,128,  0,  0, 85,170} ;
	static unsigned char blue[] =
	{  255,0,  0,  0,255,255,255,  0,  0,  0,128,255,255,128, 85,170} ;


	default_colors.ra = red ;
	default_colors.ga = green ;
	default_colors.ba = blue ;
	default_colors.n = 16 ;

	color_table( (Cint) 0, &default_colors);
}	
#endif DEAD

setlinetype(linetype)
	IXtype	linetype;
{
        Clintype        type;

        switch ((int) linetype) {

                case L_SOLID:
                        type = SOLID;
                        break;
                case L_DASH:
                        type = DASHED;
                        break;
                case L_DOT:
                        type = DOTTED;
                        break;
                case L_DASH_DOT:
                        type = DASHED_DOTTED;
                        break;
                case L_DASH_DOT_DOT:
                        type = DASH_DOT_DOTTED;
                        break;
                default:
			ct_error(NT_UPLS,"");
                        type = SOLID;
        }

        line_type(type);

}

setclipping()
{
	if (!(CLIPFLAG)) {
                /*
                 *      turn off clipping. 
                 */
		clip_indicator(NOCLIP);

        }
        else {

                /*      
		 *	else; turn on clipping  
		 */
		clip_indicator(CLIP_RECTANGLE);

		clip_rectangle((Cint) CLIPXMIN, (Cint) CLIPXMAX,
			(Cint) CLIPYMIN, (Cint) CLIPYMAX);
	}

}

#define	P_SIZE	16
#define	NN	4
static	Cint	pattern[][P_SIZE] = {
	{ 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0},	/* empty	*/
	{ 0,0,0,0, 0,0,0,0, 0,0,0,0, 1,1,1,1},	/* horizontal		*/
	{ 0,0,0,1, 0,0,0,1, 0,0,0,1, 0,0,0,1},	/* vertical		*/
	{ 0,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,0,0},	/* possitive		*/
	{ 1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1},	/* negative		*/
	{ 0,0,0,1, 0,0,0,1, 0,0,0,1, 1,1,1,1},	/* horizontal/vertical	*/
	{ 1,0,0,1, 0,1,1,0, 0,1,1,0, 1,0,0,1},	/* possitive/negative	*/
	}; /* hatch patterns	*/

setintstyle()
{

	int	i;

	switch (INT_STYLE) {
	case	HOLLOW_S:
		interior_style(HOLLOW, ON);
		break;

	case	SOLID_S:
		interior_style(SOLIDI, OFF);
		break;

	case	PATTERN_S:
		interior_style(SOLIDI, OFF);
		/*
		 *      code to invoke a pattern routine
		 */
		/*fill patterns not supported   */
		ct_error(NT_UPFS, "pattern");
		break;

	case	HATCH_S:

		switch (HATCH_IND) {

		case	HORIZONTAL:
			for (i=0; i<P_SIZE; i++)
				if (pattern[HORIZONTAL][i]) {
					pattern[HORIZONTAL][i] = 
						FILL_COLOUR.index;
				}

			pattern_table(0, NN,NN, pattern[HORIZONTAL]);
			break;

		case	VERTICAL:
			for (i=0; i<P_SIZE; i++)
				if (pattern[VERTICAL][i]) {
					pattern[VERTICAL][i] = 
						FILL_COLOUR.index;
				}

			pattern_table(0, NN,NN, pattern[VERTICAL]);
			break;

		case	POSSITIVE:
			for (i=0; i<P_SIZE; i++)
				if (pattern[POSSITIVE][i]) {
					pattern[POSSITIVE][i] = 
						FILL_COLOUR.index;
				}

			pattern_table(0, NN,NN, pattern[POSSITIVE]);
			break;

		case	NEGATIVE:
			for (i=0; i<P_SIZE; i++)
				if (pattern[NEGATIVE][i]) {
					pattern[NEGATIVE][i] = 
						FILL_COLOUR.index;
				}

			pattern_table(0, NN,NN, pattern[NEGATIVE]);
			break;

		case	HORIZ_VERT:
			for (i=0; i<P_SIZE; i++)
				if (pattern[HORIZ_VERT][i]) {
					pattern[HORIZ_VERT][i] = 
						FILL_COLOUR.index;
				}

			pattern_table(0, NN,NN, pattern[HORIZ_VERT]);
			break;

		case	POSS_NEG:
			for (i=0; i<P_SIZE; i++)
				if (pattern[POSS_NEG][i]) {
					pattern[POSS_NEG][i] = 
						FILL_COLOUR.index;
				}

			pattern_table(0, NN,NN, pattern[POSS_NEG]);
			break;
		}

		pattern_index(0);
		interior_style(HATCH, OFF);

		break;

	case	EMPTY_S:
		interior_style(HOLLOW, OFF);
		break;

	default:
		ct_error(NT_UPFS,"");
	}
}





/*
 *	color_device:
 *	PRIVATE
 *
 *		Determine if the device supports color or black 
 *	and white.
 *
 * on exit
 *	is_color	: TRUE => color else b/w
 */
Ct_err	color_device(is_color)
	boolean	*is_color;
{
	/*
	 *	The following are used to determine whether we are on
	 *	a monochrome or colour device
	 */
	int	window_fd;	/* file descriptor for the window	*/
	Pixwin	*pw;		/* pointer to windows pixwin		*/
	char	win_name[80];	/* name of the window			*/

	/* find name of window we are rendering in	*/
	if (we_getgfxwindow(win_name)) {
		ct_error(T_NULL, "Sun environment var WINDOW_GFX not set");
		return(DIE);
	}

	cmap_name = win_name;

	/* open the window				*/
	if ((window_fd = open (win_name, O_RDWR,0)) < 0) {
		ct_error(T_FOE, win_name);
		return(DIE);
	}


	/* get pixrect for that window			*/
	if ((pw = pw_open(window_fd)) == NULL ) {
		ct_error(T_FOE, "pixrect");
		(void) close(window_fd);
		return(DIE);
	}

	(void) close(window_fd);

#ifdef	SUN_OS3.5
	/*
	 *	based on the pixel depth. We will assume a depth
	 *	greater then one implies colour. See section 7.5
	 *	of the Sunview programers guide
	 */
	*is_color = pw->pixrect->pr_depth > 1 ? TRUE : FALSE;
	pw_close(pw);
	return(OK);
#else

	/*
	 *	Determine device type by type of driver loaded
	 *	See /usr/include/pixrect/*.h for details
 	 */

#ifdef mc68000
        if (pw->pw_pixrect->pr_ops == &bw1_ops) {
		*is_color = FALSE;
		pw_close(pw);
		return(OK);
	}
        if (pw->pw_pixrect->pr_ops == &cg1_ops) {
		*is_color = TRUE;
		pw_close(pw);
		return(OK);
	}
#endif
        if (pw->pw_pixrect->pr_ops == &gp1_ops) {
		*is_color = TRUE;
		pw_close(pw);
		return(OK);
	}

#ifndef	i386	/* no cg2 or cg4 on 386		*/
        if (pw->pw_pixrect->pr_ops == &cg2_ops) {
		*is_color = TRUE;
		pw_close(pw);
		return(OK);
	}
        if (pw->pw_pixrect->pr_ops == &cg4_ops) {
		*is_color = TRUE;
		pw_close(pw);
		return(OK);
	}
#else	/* 386 uses cg3 colour board		*/
        if (pw->pw_pixrect->pr_ops == &cg3_ops) {
		*is_color = TRUE;
		pw_close(pw);
		return(OK);
	}
#endif

        if (pw->pw_pixrect->pr_ops == &bw2_ops) {
		*is_color = FALSE;
		pw_close(pw);
		return(OK);
	}

	ct_error(T_FOE, "device unknown\n");
	return(DIE);
#endif
 

}
