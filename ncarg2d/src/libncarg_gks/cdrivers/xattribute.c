/*
 *	$Id: xattribute.c,v 1.1 1994-03-30 02:11:37 fred Exp $
 */
/*
 *      File:		xattribute.c
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Thu May 16 15:42:15 MDT 1991
 *
 *      Description:	This file contains routines for handling gks output
 *			attribute functions for the x device driver
 */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include "x.h"
#include "x_device.h"
#include "xddi.h"
#include "xattribute.h"
#include "text.h"
#include "transform.h"

static	int	set_foreground_color(dpy, gc, color_pal, index)
	Display		*dpy;
	GC		gc;
	Pixeltype	*color_pal;
	unsigned	index;
{
	Pixeltype	pixel;

	if (index > (MAX_COLORS - 1)) {
		return(-1);
	}

	pixel = color_pal[index];
	XSetForeground(dpy, gc, pixel);
	return(0);
}


/*ARGSUSED*/
X11_SetLinetype(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;


	int		*iptr = (int *) gksc->i.list;
	int		line_type = iptr[0];

	int		status = 0;

	XGCValues	gcv;

	static	char	
		dashlist[] = {DASHSIZE,GAPSIZE};

	static	char	
		dotlist[] = {DOTSIZE,GAPSIZE};

	static	char	
		dashdotlist[] = {
			DASHSIZE,GAPSIZE,DOTSIZE,GAPSIZE
			};

	switch (line_type) {
	
	case	SOLID_LINE:
		gcv.line_style = LineSolid;
		break;

	case	DASHED_LINE:
		XSetDashes(dpy,xi->line_gc,0,dashlist,(int) (sizeof(dashlist)));
		gcv.line_style = LineOnOffDash;
		break;

	case	DOTTED_LINE:
		XSetDashes(dpy,xi->line_gc, 0, dotlist,(int) (sizeof(dotlist)));
		gcv.line_style = LineOnOffDash;
		break;

	case	DASH_DOT_LINE:
		XSetDashes(
			dpy, xi->line_gc,0,dashdotlist,
			(int) (sizeof(dashdotlist))
			);
		gcv.line_style = LineOnOffDash;
		break;

	default:
		status = ERR_INV_LINE;
	
	}

	/* 
	 * change line GC to reflect changes
	 */
	XChangeGC(dpy, xi->line_gc, GCLineStyle, &gcv);

	return(status);
}

/*ARGSUSED*/
X11_SetLineWidthScaleFactor(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	float		*fptr = (float *) gksc->f.list;
	float		line_width = fptr[0];

	int		lw = RINT(line_width);
	unsigned long	mask;
	XGCValues	gcv;

	if (lw < 0) lw = 0;

	if (lw == 0) {
		gcv.function = GXnoop;
		mask = GCFunction;
	}
	else if (lw == 1) {
		gcv.line_width = 0;		/* draw fast 1 pixel lines */
		gcv.join_style = JoinMiter;
		gcv.function = GXcopy;
		mask = GCLineWidth | GCJoinStyle | GCFunction;
	}
	else {
		/*
		 * for fat lines change the join style to round instead of
		 * miter
		 */
		gcv.line_width = lw;
		gcv.join_style = JoinRound;
		gcv.function = GXcopy;
		mask = GCLineWidth | GCJoinStyle | GCFunction;
	}

	/*
	 * change line GC to reflect changes
	 */
	XChangeGC(dpy, xi->line_gc, mask, &gcv);

	return(0);
}


/*ARGSUSED*/
X11_SetPolylineColorIndex(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*xptr = (int *) gksc->x.list;
	unsigned	index	= (unsigned) xptr[0];

	return(set_foreground_color(dpy, xi->line_gc, xi->color_pal, index));
}

/*ARGSUSED*/
X11_SetMarkerType(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*iptr = (int *) gksc->i.list;
	int		marker_type = iptr[0];

	/*
	 * no direct support for markers in X. Have to simulate them in 
	 * software
	 */
	xi->marker_type = marker_type;

	return(0);
}


/*ARGSUSED*/
X11_SetMarkerSizeScaleFactor(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	float		*fptr = (float *) gksc->f.list;
	float		marker_size = fptr[0];

	/*
	 * no direct support for markers in X. Have to simulate them in 
	 * software
	 */
	xi->marker_size = RINT(marker_size);

	return(0);
}

/*ARGSUSED*/
X11_SetPolymarkerColorIndex(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*xptr = (int *) gksc->x.list;
	unsigned	index	= (unsigned) xptr[0];

	return(set_foreground_color(dpy, xi->marker_gc, xi->color_pal, index));
}


/*ARGSUSED*/
X11_SetTextFontAndPrecision(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*iptr = (int *) gksc->i.list;
	int	font		= iptr[0];
	int	precision	= iptr[1];

	TextAttribute	ta;
	unsigned long	mask = 0;

	ta.text_font = font;
	ta.text_precision = precision;

	mask = (TEXT_FONT_SG | TEXT_PRECISION_SG);

	return(SetTextAttribute(&ta, mask));

}

/*ARGSUSED*/
X11_SetCharacterExpansionFactor(gksc)
	GKSC	*gksc;
{
	float		*fptr = (float *) gksc->f.list;

	float		exp_factor = fptr[0];

	TextAttribute	ta;
	unsigned long	mask = 0;

	ta.char_expan_factor = exp_factor;

	mask = (CHAR_EXPAN_FACTOR_SG);

	return(SetTextAttribute(&ta, mask));
}


/*ARGSUSED*/
X11_SetCharacterSpacing(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

	float		spacing = fptr[0];

	TextAttribute	ta;
	unsigned long	mask = 0;

	ta.char_spacing = spacing;

	/*
	 * spacing is expected to be in device coordinate space => convert
	 * from NDC to DC
	 */
	spacing *= ABS(xi->transform.x_scale);

	mask = (CHAR_SPACING_SG);

	return(SetTextAttribute(&ta, mask));
}

/*ARGSUSED*/
X11_SetTextColorIndex(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*xptr = (int *) gksc->x.list;
	unsigned	index	= (unsigned) xptr[0];

	return(set_foreground_color(dpy, xi->text_gc, xi->color_pal, index));
}


/*ARGSUSED*/
X11_SetCharacterHeightAndUpVector(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

	float		x_up = fptr[0];
	float		x_base = fptr[1];
	float		y_up = fptr[2];
	float		y_base = fptr[3];

	float		height = MAG(y_up, y_base);

	TextAttribute	ta;
	unsigned long	mask = 0;

	/*
	 * height is expected to be in device coordinate space => convert
	 * from NDC to DC
	 */
	height *= ABS(xi->transform.y_scale);

	ta.orientation.x_up = x_up;
	ta.orientation.y_up = y_up;
	ta.orientation.x_base = x_base;
	ta.orientation.y_base = y_base;

	ta.char_height = height;

	mask = (CHAR_ORIENTATION_SG | CHAR_HEIGHT_SG);

	return(SetTextAttribute(&ta, mask));
}

/*ARGSUSED*/
X11_SetTextPath(gksc)
	GKSC	*gksc;
{
	int		*iptr = (int *) gksc->i.list;

	int	path	= iptr[0];

	TextAttribute	ta;
	unsigned long	mask = 0;

	ta.text_path = path;

	mask = (TEXT_PATH_SG);

	return(SetTextAttribute(&ta, mask));
}


/*ARGSUSED*/
X11_SetTextAlignment(gksc)
	GKSC	*gksc;
{
	int		*iptr = (int *) gksc->i.list;

	int	horiz_align	= iptr[0];
	int	vert_align	= iptr[1];

	TextAttribute	ta;
	unsigned long	mask = 0;

	ta.text_alignment.horizontal = horiz_align;
	ta.text_alignment.vertical = vert_align;

	mask = (TEXT_ALIGNMENT_SG);

	return(SetTextAttribute(&ta, mask));
}

/*ARGSUSED*/
X11_SetFillAreaInteriorStyle(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*iptr = (int *) gksc->i.list;

	int	fill_style	= iptr[0];

	xi->fill_style = fill_style;

	return(0);
}


/*ARGSUSED*/
X11_SetFillAreaStyleIndex(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*iptr = (int *) gksc->i.list;

	int	hatch_index	= iptr[0];

	xi->hatch_index = hatch_index;

	return(0);
}

/*ARGSUSED*/
X11_SetFillAreaColorIndex(gksc)
	GKSC	*gksc;
{
	Xddp    *xi = (Xddp *) gksc->ddp;
	Display *dpy = xi->dpy;
	Window  win = xi->win;

	int		*xptr = (int *) gksc->x.list;
	unsigned	index	= (unsigned) xptr[0];

	return(set_foreground_color(dpy, xi->fill_gc, xi->color_pal, index));

}


/*ARGSUSED*/
X11_SetColorRepresentation(gksc)
	GKSC	*gksc;
{
        Xddp    	*xi = (Xddp *) gksc->ddp;
        Display 	*dpy = xi->dpy;
        Window  	win = xi->win;
	Colormap	cmap = xi->cmap;

	int		*xptr = (int *) gksc->x.list;
	XColor		*rgbptr = (XColor *) gksc->rgb.list;

	unsigned	index	= (unsigned) xptr[0];
	Pixeltype	*color_pal = xi->color_pal;
	
	Pixeltype	planedummy[1];		/* not used	*/
	Pixeltype	pixel_return[1];	/* device index	*/
	Visual		*visual;

	if (index > (MAX_COLORS - 1)) {
		return(-1);
	}

	if (! xi->color_ava) return(0);	/* not a color device	*/

	visual = DefaultVisual(dpy, DefaultScreen(dpy));

	/*
	 * see if we have a read only color model
	 */
	if ((visual->class == TrueColor) || (visual->class == StaticColor)
		|| (visual->class == StaticGray)) {


                if (!XAllocColor(dpy, cmap, &rgbptr[0])) {

			/* error allocating color cell	*/
                        return(-1);
		}

		color_pal[index] = rgbptr->pixel;

		/*
		 * if index == 0 then change background color
		 */
		if (index == 0) {
			XSetWindowBackground(dpy, win, rgbptr->pixel);
			XClearWindow(dpy, win);
		}

		return (0);
	}

	/*
	 * must have read/write color model
	 */
 
	/*
	 * if this index has not had a cell allocated to it previously
	 * we need to do it now. It hasn't been allocated previsously if
	 * its pointing to the foreground pixel, color_pal[1]. Initially all
	 * entries in color_pal (except color_pal[0], background color) are
	 * pointing to the default foreground pixel.
	 */
	if (index !=1 && color_pal[index]==WhitePixel(dpy,DefaultScreen(dpy))){
		/*
		 * try and alloc a new cell in the color map
		 */
		if (XAllocColorCells(dpy,cmap,FALSE, planedummy,
				0, pixel_return, 1) == 0) {

			/* error allocating color cell	*/
			return (-1);
		}

		/* 
		 *	record pixel in the colortable
		 */
		color_pal[index] = pixel_return[0];
	}

	/* 
	 *	set cell index in the colour map
	 */
	rgbptr->pixel = color_pal[index];

	/* 
	 *	store the colour in the map
	 */
	XStoreColor(dpy, cmap, &rgbptr[0]);

	/*
	 * if index == 0 then change background color
	 */
	if (index == 0) {
		XSetWindowBackground(dpy, win, rgbptr->pixel);
		XClearWindow(dpy, win);
	}

	return(0);
}

/*ARGSUSED*/
X11_SetClipIndicator(gksc)
	GKSC	*gksc;
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;
        Window  win = xi->win;

	int		*iptr = (int *) gksc->i.list;
	XPoint	*pptr = (XPoint *) gksc->p.list;

	int		clip_flag = iptr[0];
	XPoint	*llptr = &pptr[0];
	XPoint	*urptr = &pptr[1];

	XRectangle rect;

	if (clip_flag) {

		/*
		 * turn clipping on
		 */

		/*
		 * X has origin at upper left, NDC space has origin at
		 * lower left
		 */
		rect.x = llptr->x;
		rect.y = urptr->y;
		rect.width = urptr->x - llptr->x + 1;
		rect.height = llptr->y - urptr->y + 1;

		XSetClipRectangles(dpy, xi->line_gc, 0, 0, &rect, 1, Unsorted);
		XSetClipRectangles(dpy, xi->marker_gc, 0, 0, &rect, 1,Unsorted);
		XSetClipRectangles(dpy, xi->text_gc, 0, 0, &rect, 1, Unsorted);
		XSetClipRectangles(dpy, xi->fill_gc, 0, 0, &rect, 1, Unsorted);
		XSetClipRectangles(dpy, xi->cell_gc, 0, 0, &rect, 1, Unsorted);
		XSetClipRectangles(dpy, xi->bg_gc, 0, 0, &rect, 1, Unsorted);

	}
	else {
		/*
		 * turn clipping off
		 */
		XSetClipMask(dpy, xi->line_gc, (Pixmap) None);
		XSetClipMask(dpy, xi->marker_gc, (Pixmap) None);
		XSetClipMask(dpy, xi->text_gc, (Pixmap) None);
		XSetClipMask(dpy, xi->fill_gc, (Pixmap) None);
		XSetClipMask(dpy, xi->cell_gc, (Pixmap) None);
		XSetClipMask(dpy, xi->bg_gc, (Pixmap) None);
	}

	return(0);
}


/*ARGSUSED*/
X11_SetWindow(gksc)
	GKSC	*gksc;
{
        Xddp    	*xi = (Xddp *) gksc->ddp;
	float		*fptr = (float *) gksc->f.list;

	float		llx = fptr[0];
	float		urx = fptr[1];
	float		lly = fptr[2];
	float		ury = fptr[3];

	if ((llx == urx) || (lly == ury)) {
		return (ERR_INV_RECT);
	}

	TransformSetWindow(&xi->tsystem, llx, lly, urx, ury);
	xi->transform = TransformGetTransform(&xi->tsystem);

	return(0);
}

/*ARGSUSED*/
X11_SetViewport(gksc)
	GKSC	*gksc;
{
        Xddp    	*xi = (Xddp *) gksc->ddp;
	float		*fptr = (float *) gksc->f.list;

	float		llx = fptr[0];
	float		urx = fptr[1];
	float		lly = fptr[2];
	float		ury = fptr[3];

	if ((llx == urx) || (lly == ury)) {
		return (ERR_INV_RECT);
	}

	TransformSetViewport(&xi->tsystem, llx, lly, urx, ury);
	xi->transform = TransformGetTransform(&xi->tsystem);

	return(0);
}

