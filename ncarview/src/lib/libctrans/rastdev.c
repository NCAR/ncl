/*
 *	$Id: rastdev.c,v 1.8 1992-04-03 20:58:08 clyne Exp $
 */
#include <stdio.h>
#include <ncarg_ras.h>
#include <cterror.h>
#include "rast.h"
#include "cgmc.h"
#include "default.h"
#include "ctrandef.h"
#include "translate.h"

extern	Raster	*rastGrid;

static	int	rasColorIndex;

static	float	rasLineWidth; 

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
	void	line_();
	Ct_err	ComLineSim();

	if (LINE_TYPE != 1) {
		for(i=1;i<*coord_buf_num;i++)
			(void) ComLineSim(coord_buf[i-1].x,coord_buf[i-1].y,
			coord_buf[i].x,coord_buf[i].y);

	
		return;
	}

	/*
	 * use fat line algo for linewidths greater then 1.
	 */
	if (rasLineWidth == 0.0) {
		return;	/* don't draw 0 width lines	*/
	} 
	else if (rasLineWidth <= 1.5) {
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
			coord_buf[i].x, coord_buf[i].y, ROUND(rasLineWidth));
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
	void	line_();

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
	void	line_();

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
	rasLineWidth = line_width;
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


		
		


/*
 * line_:	scan convert a line from (x1, y1) to (x2, y2), using
 *		the macro PUTPIX to put each pixel. Does no clipping. Uses
 *		Breseham's algorithm. Taken from "Graphics Gems"
 */
static	void	line_(x1, y1, x2, y2)
	int	x1, y1, x2, y2;
{
	int	d, x, y, ax, ay, sx, sy, dx, dy;

	dx = x2-x1; ax = ABS(dx)<<1; sx = SIGN(dx);
	dy = y2-y1; ay = ABS(dy)<<1; sy = SIGN(dy);

	x = x1;
	y = y1;
	if (ax > ay) {	/*	x dominant	*/
		d = ay - (ax>>1);
		for (;;) {
			INDEXED_PIXEL(rastGrid, x, y) = rasColorIndex;
			if (x == x2) return;
			if (d >= 0) {
				y += sy;
				d -= ax;
			}
			x += sx;
			d += ay;
		}
	}
	else {	/*	y dominant	*/
		d = ax-(ay>>1);
		for (;;) {
			INDEXED_PIXEL(rastGrid, x, y) = rasColorIndex;
			if (y == y2) return;
			if (d >= 0) {
				x += sx;
				d -= ay;
			}
			y += sy;
			d += ax;
		}
	}
}


void	rast_update_color_table()
{
	int	i;

	extern	RasColrTab      colorTab;

	/*
	 * any time we change the colour table we "damage" the colour
	 * attributes
	 */
	FILL_COLOUR_DAMAGE = TRUE;
	MARKER_COLOUR_DAMAGE = TRUE;
	LINE_COLOUR_DAMAGE = TRUE;

	for (i=0; COLOUR_TOTAL_DAMAGE > 0 && i<=MAX_C_I && i<MAX_COLOR; i++) {

		if (COLOUR_INDEX_DAMAGE(i)) {
			colorTab.rgb[i].red =  COLOUR_INDEX_RED(i);
			colorTab.rgb[i].green =  COLOUR_INDEX_GREEN(i);
			colorTab.rgb[i].blue =  COLOUR_INDEX_BLUE(i);

			COLOUR_TOTAL_DAMAGE--;
			COLOUR_INDEX_DAMAGE(i) = FALSE;

		}
	}
}
