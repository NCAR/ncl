#include <stdio.h>
#include <raster.h>
#include "rast.h"
#include "cgmc.h"
#include "default.h"
#include "ctrandef.h"
#include "translate.h"

extern	Raster	*rastGrid;

static	int	rasColorIndex;

static	int	rasLineWidth; 

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

	if (LINE_TYPE != 1) {
		for(i=1;i<*coord_buf_num;i++)
			(void) ComLineSim(coord_buf[i-1].x,coord_buf[i-1].y,
			coord_buf[i].x,coord_buf[i].y);

	
		return;
	}

	/*
	 * use fat line algo for linewidths greater then 1.
	 */
	if (rasLineWidth == 1) {
		for (i=1; i<*coord_buf_num; i++) {
		line_(XConvert(coord_buf[i-1].x), YConvert(coord_buf[i-1].y), 
			XConvert(coord_buf[i].x), YConvert(coord_buf[i].y));
		}
	}
	else {
		for (i=1; i<*coord_buf_num; i++) {
		ComFatLine(coord_buf[i-1].x, coord_buf[i-1].y, 
			coord_buf[i].x, coord_buf[i].y, rasLineWidth);
		}
	}
}

/*
 *      Take two world coords , clip and draw line
 */
void	rast_line(x1_,y1_,x2_,y2_)
	long	x1_,y1_,x2_,y2_;
{

	int	i;
	long	x1, y1, x2, y2;
	void	line_();

	if (!(Clipper(x1_, y1_, x2_, y2_, &x1, &y1, &x2, &y2))) {
		return;
	}

	line_(XConvert(x1), YConvert(y1), XConvert(x2), YConvert(y2));
}

/*
 *      Take two device coords and draw line (no clipping) 
 */
void	rast_devline(x1_,y1_,x2_,y2_)
	long	x1_,y1_,x2_,y2_;
{
	void	line_();

	line_(x1_, y1_, x2_, y2_);
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
	rasLineWidth = (int) line_width;
}

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
