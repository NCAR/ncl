/*
 *	$Id: X_cell.c,v 1.2 1991-01-09 11:07:48 clyne Exp $
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
#include	<stdio.h>
#include        <X11/Xlib.h>
#include        <X11/Xutil.h>
#include        <math.h>
#include	<ncarv.h>
#include	<cterror.h>
#include	"cgmc.h"
#include	"ctrandef.h"
#include	"Xdefs.h"
/*	X_cell.c:
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *				10/20/88
 *
 *
 *	This file contains the cell array simulation routine  for X-ctrans.
 *	Note:
 *	in order to ensure proper size boundries of a cell array not all
 *	cells will be the same size in general. They will only differ 
 *	however, by one pixel. If the cell array is rectangular then raster
 *	instruction will be used else cells will be simulated with fill
 *	polygons.
 */

extern	Pixeltype	Colortab[];
extern	boolean		Colordef[];
extern	Ct_err		Instr_Dec();
extern	boolean		Color_ava;

/*
 *	macro for calculating the size of a XImage data field
 *	This macro assumes that a 8 bit display will require one
 *	byte per pixel. A 16 bit requires 2 bytes etc. This may
 *	not be correct but it works so far.
 */
#define	RASTER_SIZE(R, D)	(unsigned) (((R).height * (R).width) * (float) \
				(D) / (float) sizeof(char))
#define	HEIGHT	10
typedef	struct {
	Pixeltype	*p;	/* raster array of pixel values		*/
	int	index,		/* index into p				*/
		x,y,		/* current possision in array		*/
		height,		/* height of array			*/
		width;		/* width of array			*/
	} Raster; 		/* we buffer up raster images until 
				 * they're HEIGHT x raster.width in size
				 */


/*	raster_packed_cell_sim:
 *
 *		simulate a cell array using XImages that have a "packed"
 *	encoding. See discussion on Cell arrays in "NCAR Graphics installation
 *	guide"
 *
 * on entry:
 *	c		: the CGMC containing the data
 *	P		: the 'P' coordinate of a rectangular cell array
 *	*rows		: a list of the number of pixels in each cell in a row
 *	*cols		: a list of the number of pixels in each cell in a cols
 *	nx		: number of colums in cell array
 *	ny		: number of rows in cell array
 *	width		: width of the cell array in pixels.
 */
Ct_err	raster_packed_cell_sim(c, P, rows, cols, nx, ny, width)

	CGMC	*c;
	Ptype	P;
	int	*rows, *cols;
	int	nx, ny;
	int	width;
{

	Raster	raster;
	XImage	*image;

	register	int k,l;
	register	int i,j;

	Pixeltype *index_array = NULL;	/* single row of cell pixel vals*/
	unsigned char	*image_buf = NULL;/* memory for XImage structure*/

	int	index = 0;      /* index for color list in cgmc */

	raster.width =  width; 
	raster.height = HEIGHT;
	raster.x = 0;
	raster.y = HEIGHT - 1;

	raster.p = (Pixeltype *) icMalloc 
		((unsigned) (raster.width * raster.height) * 
		sizeof(Pixeltype));

	index_array = (Pixeltype *) icMalloc 
		((unsigned) nx * sizeof(Pixeltype));

	/*
	 *	alloc memory for data in image
	 */
	image_buf = (unsigned char *) icMalloc (RASTER_SIZE(raster,DspDepth));

			

	/*
	 *	create the XImage structure for raster instructions
	 */
	if((image = XCreateImage(dpy, visual, DspDepth, ZPixmap, 0,
		(char *) image_buf,
		 raster.width, raster.height, 8, 0)) == NULL) {

		ct_error(NT_MALLOC,"");
		return (SICK);
	}



	init_putpixel(image);

	P.y -= raster.height;	/* cell array origin is at  bottom left.
				 * XImage origin  is at top left
				 */	



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
		for (j=0; j < rows[i]; j++, raster.y--) {

			raster.x = 0;

			/*
			 * calculate index to map 2D cell array into 1D 
			 * raster array
			 */
			raster.index =  raster.width * raster.y;

			/*	the coloumns	*/
			for (k=0; k<nx; k++) {


				/*	the coloums of pixels per cell	*/
				for (l=0; l< cols[k]; l++, raster.index++ ) {
					raster.p[raster.index] = index_array[k];
	
				}
			}

			/* if raster array is full then render it	*/
			if (raster.y == 0) {

				/*
				 * convert pixel values into XImage data format
				 */
				build_image(image, raster.p);

				XPutImage(dpy, drawable, cellGC, image, 0,0, 
					P.x, P.y, raster.width, raster.height);

				raster.y = raster.height;
				P.y -= raster.height;
			}
		}

	
	}

	/* 
	 *	flush buffer in necessary
	 */
	if (raster.y != (raster.height - 1)) {
		/*
		 * convert pixel values into XImage data format
		 */
		build_image(image, raster.p);

		XPutImage(dpy, drawable, cellGC, image, 0,(raster.y + 1), 
			P.x, (P.y + raster.y + 1), raster.width, 
			(raster.height - (raster.y + 1)));
	}


	XDestroyImage(image);

	if (index_array != (Pixeltype *) NULL) 
		cfree((char *) index_array);
	if (raster.p != (Pixeltype *) NULL) 
		cfree((char *) raster.p);

	return (OK);
}

/*
 *	polygon_packed_cell_sim:
 *
 *		simulate non-rectangular cell arrays using polygons.
 *
 * on entry:
 *		args are same as for raster_packed_cell_sim
 */
Ct_err	polygon_packed_cell_sim(c, P, delta_pr_x, delta_pr_y, delta_qr_x,
			delta_qr_y, fudge_x, fudge_y, nx, ny)

	CGMC	*c;
	Ptype	P;
	short	delta_pr_x,
		delta_pr_y,
		delta_qr_x,
		delta_qr_y;
	int	fudge_x, fudge_y;
	int	nx, ny;
{

	register	int i,j;

	int	inc_y, 
		inc_x;

	int	fudge_y_counter,
		fudge_x_counter;

	int	index = 0;

	fudge_y_counter = fudge_y;
	inc_y = (fudge_y ? -1 : 0);
	for (i=0;i<ny;i++) {


		Points.P[0].y = P.y;
		Points.P[0].x = P.x;

		fudge_x_counter = fudge_x;
		inc_x = (fudge_x ? 1 : 0);

		for (j=0;j<nx;j++, index++ ) {


			Points.P[1].x = Points.P[0].x + delta_pr_x
					+ inc_x;

			Points.P[1].y = Points.P[0].y + delta_pr_y;

			Points.P[2].x = Points.P[1].x + delta_qr_x;
			Points.P[2].y = Points.P[1].y + delta_qr_y
					+ inc_y;

			Points.P[3].x = Points.P[0].x + delta_qr_x;
			Points.P[3].y = Points.P[0].y + delta_qr_y
					+ inc_y;

			if (Color_ava) {

				if (index == c->Cnum && c->more) {
					if (Instr_Dec(c) != OK)
						return (pre_err);

					index = 0;
				}

				/* make sure color is defined	*/
				if (Colordef[c->c[index]])	{

					XSetForeground(dpy, cellGC, 
						Colortab[c->c[index]]); 
				}
				else
					XSetForeground(dpy, cellGC, 
						Colortab[1]); 


				XFillPolygon(dpy, drawable, cellGC,
					Points.P, 4, Complex,
					CoordModeOrigin); 
			}
			else {	/* just draw a box	*/
				XDrawLines(dpy, drawable, cellGC,
					Points.P, 4,
					CoordModeOrigin);
			}

			Points.P[0].x += delta_pr_x + inc_x;
			Points.P[0].y += delta_pr_y;

			if (fudge_x_counter > 0) {
				fudge_x_counter--;
			}
			else
				inc_x = 0;
				

		}

		P.x += delta_qr_x;
		P.y += delta_qr_y + inc_y;

		if (fudge_y_counter > 1) {
		fudge_y_counter--;
		}
		else
			inc_y = 0;

	}

	return (OK);
}
