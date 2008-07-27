/*
 *	$Id: rastdev.c,v 1.21 2008-07-27 03:18:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1993                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <ncarg/ncarg_ras.h>
#include "rast.h"
#include "cgmc.h"
#include "default.h"
#include "ctrandef.h"
#include "translate.h"

extern	Raster	*rastGrid;
extern	boolean	rasIsDirect;
extern	boolean	startedDrawing;

static	int	rasColorIndex;

static	int	rasLineWidth; 

/*
 * line_:	scan convert a line from (x1, y1) to (x2, y2), using
 *		the macro PUTPIX to put each pixel. Does no clipping. Uses
 *		Breseham's algorithm. Taken from "Graphics Gems"
 */
static	void	line_(x1, y1, x2, y2)
	int	x1, y1, x2, y2;
{

	int	dx, dy;		/* difference between endpoints	*/
	int	ix, iy;		/* absolute value of (dx, dy)	*/
	int	i, inc;		/* the largest of ix, iy	*/
	int	x,y;
	int	plotx, ploty;	/* possible candiated for ploting	*/
	int	index = rasColorIndex;
	int	do_plot = 0;
	int	temp;

	extern	RasColrTab      colorTab;

	/*
	 * bogus algorithm draws lines differently from top to bottom
	 * the from bottom to top
	 */
	if (y2 > y1) {
		temp = x1; x1 = x2; x2 = temp;
		temp = y1; y1 = y2; y2 = temp;
	}


	dx = x2 - x1;
	dy = y2 - y1;

	ix = ABS(dx);
	iy = ABS(dy);
	inc = MAX(ix, iy);

	plotx = x1;
	ploty = y1;
	x = y = 0;

	RAS_PUT_PIX(rastGrid,plotx,ploty,index,colorTab,rasIsDirect);

	for (i = 0; i <= inc; ++i) {
		x += ix;
		y += iy;

		do_plot = 0;	/* false	*/

		if (x > inc) {
			do_plot = 1;
			x -= inc;
			plotx = (dx > 0
				? plotx + 1 : (plotx == 0 ? plotx : plotx - 1));


		}

		if (y > inc) {
			do_plot = 1;
			y -= inc;
			ploty = (dy > 0 
				? ploty + 1 : (ploty == 0 ? ploty : ploty - 1));
		}


		if (do_plot) {
			RAS_PUT_PIX(rastGrid,plotx,ploty,index,colorTab,rasIsDirect);
		}
	}

}
void	rast_open(max_poly_points)
	unsigned long	*max_poly_points;
{
	*max_poly_points = ~0L;
}

void	rast_close()
{
}


/*VARARGS*/
void	rast_pointflush(coord_buf, coord_buf_num)
	Ptype	*coord_buf;
	long	*coord_buf_num;
{
	int	i;
	int	ComLineSim();

	if (LINE_TYPE != 1) {
		for(i=1;i<*coord_buf_num;i++)
			(void) ComLineSim(coord_buf[i-1].x,coord_buf[i-1].y,
			coord_buf[i].x,coord_buf[i].y);

	
		return;
	}

	/*
	 * use fat line algo for linewidths greater then 1.
	 */
	if (rasLineWidth == 0) {
		return;	/* don't draw 0 width lines	*/
	} 
	else if (rasLineWidth == 1) {
		for (i=1; i<*coord_buf_num; i++) {
		line_((int) XConvert(coord_buf[i-1].x), 
			(int) YConvert(coord_buf[i-1].y), 
			(int) XConvert(coord_buf[i].x), 
			(int) YConvert(coord_buf[i].y));
		}
	}
	else {
		for (i=1; i<*coord_buf_num; i++) {
		ComFatLine(coord_buf[i-1].x, coord_buf[i-1].y, 
			coord_buf[i].x, coord_buf[i].y, rasLineWidth, 1);
		}
	}
}

/*
 *      Take two world coords , clip and draw line
 */
void	rast_line(x1_,y1_,x2_,y2_)
	long	x1_,y1_,x2_,y2_;
{

	long	x1, y1, x2, y2;

	if (!(Clipper(x1_, y1_, x2_, y2_, &x1, &y1, &x2, &y2))) {
		return;
	}

	line_((int) XConvert(x1), (int) YConvert(y1), 
		(int) XConvert(x2), (int) YConvert(y2));
}

/*
 *      Take two device coords and draw line (no clipping) 
 */
void	rast_devline(x1_,y1_,x2_,y2_)
	long	x1_,y1_,x2_,y2_;
{

	line_((int) x1_, (int) y1_, (int) x2_, (int) y2_);
}

void	rast_linestyle()
{
}

void	rast_linecolour(index)
	CItype	index;
{
	rasColorIndex = (int) index;
}

void	rast_fillcolour(index)
	CItype	index;
{
	rasColorIndex = (int) index;
}

void	rast_linewidth(line_width)
	Rtype	line_width;
{
	line_width = line_width < 0.0 ? 0.0 : line_width;
	rasLineWidth = (int) ROUND(line_width);
}

#ifdef	DEAD
/*
 * line_:	scan convert a line from (x1, y1) to (x2, y2), using
 *		the macro PUTPIX to put each pixel. Does no clipping. Uses
 *		Symmetric Double Step Breseham's algorithm. Taken from 
 *		"Graphics Gems"
 */
#define	SWAP(a,b)	{a^=b; b^=a; a^=b;}
#define	ABSOLUTE3(i,j,k)	( (i-j) * (k = ( (i-j)<0 ? -1 : 1)))
#define	PLOT(x,y,f)	if (f) \
		INDEXED_PIXEL((rastGrid), (y), (x)) = (rasColorIndex); \
	else \
		INDEXED_PIXEL((rastGrid), (x), (y)) = (rasColorIndex);
static	void	line_(a1, b1, a2, b2)
	int	a1, b1, a2, b2;
{
	int	dx, dy, incr1, incr2, D, x, y, xend, c, pixels_left;
	int	x1, y1;
	int	sign_x, sign_y, step, reverse, i;

	dx = ABSOLUTE3(a2, a1, sign_x);
	dy = ABSOLUTE3(b2, b1, sign_y);

	/*
	 * symetric double-step draws a line instead of a point if line
	 * is actually a point.
	 */
	if (a1 == a2 && a1 == b1 && a1 == b2) {
		PLOT(a1, a1, 1);
		return;
	}

	/*
	 * decide increment sign by the slope sign
	 */
	if (sign_x == sign_y) step = 1; else step = -1;

	/*
	 * choose axis of greates movement
	 */
	if (dy > dx) {
		SWAP(a1, b1);
		SWAP(a2, b2);
		SWAP(dx, dy);
		reverse = 1;
	}
	else {
		reverse = 0;
	}

	if (a1 > a2) {
		x = a2;
		y = b2;
		x1 = a1;
		y1 = b1;
	}
	else {
		x = a1;
		y = b1;
		x1 = a2;
		y1 = b2;
	}

	/*
	 * note; dx=n => 0 - n or (dx+1) pixels to be set. Go round loop
	 * dx/4 times then PLOT last 0,1,2, or 3 pixels. In fact (dx-1)/4 as 
	 * 2 pixels are already PLOTted
	 */
	xend = (dx - 1) / 4;
	pixels_left = (dx - 1) % 4;

	PLOT(x,y,reverse);
	PLOT(x1, y1,reverse);

	incr2 = 4 * dy - 2 * dx;
	if (incr2 < 0) {	/* slope less then 1/2	*/
		c = 2 * dy;
		incr1 = 2 * c;
		D = incr1 - dx;
		
		for (i=0; i<xend; i++) {
			++x;
			--x1;
			if (D < 0) {

				PLOT(x,y,reverse);	/* pattern 1, forward*/
				PLOT(++x,y,reverse);	/* pattern 1, backword*/
	
				PLOT(x1,y1,reverse);	
				PLOT(--x1,y1,reverse);
				D += incr1;
			} 
			else {
				if (D < c) {
					/* pattern 2, forwards	*/
					PLOT(x,y,reverse);	
					PLOT(++x,y += step,reverse);
					/* pattern 2, backwords	*/
					PLOT(x1,y1,reverse);	
					PLOT(--x1,y1 -= step,reverse);
				}
				else {
					/* pattern 3, forwards	*/
					PLOT(x,y += step,reverse);	
					PLOT(++x,y,reverse);
					/* pattern 3, backwords	*/
					PLOT(x1,y1 -= step,reverse);	
					PLOT(--x1,y1,reverse);
				}
				D += incr2;
			}
		}

		/*
		 * PLOT last pattern
		 */
		if (pixels_left) {
			if (D < 0) {
				PLOT(++x, y, reverse);	/* pattern 1	*/
				if (pixels_left > 1)
					PLOT(++x,y,reverse);
				if (pixels_left > 2)
					PLOT(--x1,y1,reverse);
			}
			else {
				if (D < c) {
					PLOT(++x,y,reverse);	/* pat 2 */
					if (pixels_left > 1)
						PLOT(++x,y += step,reverse);
					if (pixels_left > 2)
						PLOT(--x1,y1,reverse);
				}
				else {	/* pattern 3	*/
					PLOT(++x,y += step,reverse);
					if (pixels_left > 1)
						PLOT(++x,y,reverse);
					if (pixels_left > 2)
						PLOT(--x1,y1 -= step,reverse);
				}
			}
		}
	}
	/*
	 * end slope < 1/2
	 */
	else {	/* slope > 1/2	*/
		c = 2 * (dy - dx);
		incr1 = 2 * c;
		D = incr1 + dx;

		for (i=0; i<xend; i++) {
			++x;
			--x1;

			if (D > 0) {
				/* pattern 4, forwards		*/
				PLOT(x,y+= step,reverse);
				PLOT(++x,y+= step,reverse);
	
				/* pattern 4, backwords	*/
				PLOT(x1,y1 -= step,reverse);	
				PLOT(--x1,y1 -= step,reverse);	
				D += incr1;
			}
			else {
				if (D < c) {
					/* pattern 2, forwards		*/
					PLOT(x,y,reverse);
					PLOT(++x,y+= step,reverse);
		
					/* pattern 2, backwords	*/
					PLOT(x1,y1,reverse);	
					PLOT(--x1,y1 -= step,reverse);	
				}
				else {
					/* pattern 3, forwards		*/
					PLOT(x,y += step,reverse);
					PLOT(++x,y,reverse);
		
					/* pattern 3, backwords	*/
					PLOT(x1,y1 -= step,reverse);	
					PLOT(--x1,y1,reverse);	
				}
				D += incr2;
			}
		}

		/*
		 * PLOT last pattern
		 */
		if (pixels_left) {
			if (D > 0) {
				PLOT(++x, y += step, reverse);	/* patt 4*/
				if (pixels_left > 1)
					PLOT(++x,y += step,reverse);
				if (pixels_left > 2)
					PLOT(--x1,y1 -= step,reverse);
			}
			else {
				if (D < c) {
					PLOT(++x,y,reverse);	/* pat 2 */
					if (pixels_left > 1)
						PLOT(++x,y += step,reverse);
					if (pixels_left > 2)
						PLOT(--x1,y1,reverse);
				}
				else {	/* pattern 3	*/
					PLOT(++x,y += step,reverse);
					if (pixels_left > 1)
						PLOT(++x,y,reverse);
					if (pixels_left > 2)
						if (D > c) {
						PLOT(--x1,y1 -= step,reverse);
						}
						else {
						PLOT(--x1,y1,reverse);
						}
				}
			}
		}
	}
}
#endif


		
		

extern	RasColrTab      colorTab;

int	set_back_colr() {

	CGMC	cgmc;

	if (startedDrawing) {
		ESprintf(E_UNKNOWN, "Background color changes ignored after drawing has begun");
		return(-1);
	}

	colorTab.rgb[0].red =  COLOUR_INDEX_RED(0);
	colorTab.rgb[0].green =  COLOUR_INDEX_GREEN(0);
	colorTab.rgb[0].blue =  COLOUR_INDEX_BLUE(0);

	Ras_ClearDevice(&cgmc);

	return(0);

}


int	rast_update_color_table()
{
	int	i;
	int	status = 0;


	/*
	 * any time we change the colour table we "damage" the colour
	 * attributes
	 */
	FILL_COLOUR_DAMAGE = TRUE;
	MARKER_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;

	/*
	 * This is a hack to ensure background color gets set correctly
	 * in the case that colr table index 0 is changed *and* no
	 * coresponding CGM BACKGROUND COLOUR is received
	 */
	if (COLOUR_INDEX_DAMAGE(0)) {
		if (set_back_colr() < 0) status = -1;
		COLOUR_TOTAL_DAMAGE--;
		COLOUR_INDEX_DAMAGE(0) = FALSE;
	}

	for (i=1; COLOUR_TOTAL_DAMAGE > 0 && i<=MAX_C_I && i<MAX_COLOR; i++) {

		if (COLOUR_INDEX_DAMAGE(i)) {
			colorTab.rgb[i].red =  COLOUR_INDEX_RED(i);
			colorTab.rgb[i].green =  COLOUR_INDEX_GREEN(i);
			colorTab.rgb[i].blue =  COLOUR_INDEX_BLUE(i);

			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;

		}
	}
	return(status);
}
