/*
 *	$Id: cgi_cell_sim.c,v 1.4 1991-09-26 16:29:03 clyne Exp $
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
#include	<cgidefs.h>
#include	"cgmc.h"
#include	<cterror.h>
#include	"ctrandef.h"


/*	cgi_cell_sim.c:
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *				10/20/88
 *
 *
 *	This file contains the cell array simulation routine  for ctrans_CGI.
 *	Cell arrays are simulated rather then using the CGI cell array 
 *	primitive to allow more control and avoid size constraints. Note
 *	in order to ensure proper size boundries of a cell array not all
 *	cells will be the same size in general. They will only differ 
 *	however, by one pixel. If the cell array is rectangular then raster
 *	instructions will be used. Else cells will be simulated with fill
 *	polygons.
 */



static	struct {
	Cint	*colorind;	/* raster array of colour indecies	*/
	int	size;		/* num alloced to colorind		*/
	} craster = {NULL, 0};

/*	cgi_packed_cell:
 *
 *		simulate a cell array using polygons that has a "packed"
 *	encoding. See discussion on Cell arrays in "NCAR Graphics installation
 *	guide"
 *
 *	on entry:
 *		parameters are as documented in the routine CGICellArray
 *		in cgi.c
 */
Ct_err
cgi_packed_cell_sim(c, P, delta_pr_x, delta_pr_y, delta_qr_x, delta_qr_y,
		fudge_x, fudge_y, nx, ny)

	CGMC	*c;
	Ccoor	P;
	Cint	delta_pr_x,
		delta_pr_y,
		delta_qr_x,
		delta_qr_y;
	int	fudge_x, fudge_y;
	int	nx, ny;
{

	/* raster variables	*/
	Ccoor	pcell;		/* base	coordinates of raster array	*/
	Cint	c_ptr, 		/* index into raster array		*/
		k,l;
	Cint	len;		/* width of cell array in pixels	*/
	


	/* polygon simulation variables	*/
	Ccoorlist	cell;	/* polygon used to draw a cell	*/

	/*	vars needed to "fudge" cell size for proper boundry size */
	Cint	inc_x,
		inc_y;
	int	fudge_x_counter,
		fudge_y_counter;


	int	index = 0;      /* index for color list in cgmc */

	register
		int	i,j;





	fudge_y_counter = fudge_y;
	inc_y = (fudge_y ? 1 : 0);

	/*
	 * see if cell array is a rectangle. we can use a faster algorithm
	 * if it is. Large cell arrays take forever and ever to render.
 	 * So we want to be as quick as possible.
	 */


	if ( delta_pr_y == 0 && delta_qr_x == 0) {

		/* rectangular cell array so use raster instructions	*/

		len = (nx * delta_pr_x) + fudge_x;
		if (craster.size < len) {
			if (craster.colorind != (Cint *) NULL) 
				cfree((char *) craster.colorind);
			craster.colorind = 
				(Cint *) malloc ((unsigned) len * sizeof (Cint));

			craster.size = len;
		}

		pcell.x = P.x;
		pcell.y = P.y + 1;	/* raster lines seem to be off by one */

		/* the columns	*/ 
		for (i=0;i<ny;i++) {


			fudge_x_counter = fudge_x;
			inc_x = (fudge_x ? 1 : 0);

			c_ptr = 0;

			/*	render a row of cells	*/
			for (j=0;j<nx;j++, index++ ) {


				/* 
				 * make sure data available in cgmc
				 * This statement will only evaluate to
				 * true when rendering super big cell arrays
				 */
				if (index == c->Cnum && c->more) {
					if (Instr_Dec(c) < 1) {
						ct_error(T_FRE, "metafile");
						return (DIE);
					}

					index = 0;
				}

			/* load raster array with a cell value for 
			 * a single row of pixels
			 */	
			for (k=0;k < delta_pr_x + inc_x; k++, c_ptr++)
				craster.colorind[c_ptr] = c->c[index];


				/* see if we're still fudging	*/
				if (fudge_x_counter > 1) {
					fudge_x_counter--;
				}
				else
					inc_x = 0;
					

			}

			/* draw row of cells one pixel row at a tiime	*/ 
			for (l=0; l < delta_qr_y + inc_y; l++)  {
				pixel_array(&pcell, c_ptr, 1, craster.colorind);
				pcell.y++;
			}


			if (fudge_y_counter > 1) {
				fudge_y_counter--;
			}
			else
				inc_y = 0;

		}
	}

	else {

		/* 
		 *	skewed cell array (parallelagram). simulate using
		 * 	polygons. this can be very slow.
		 */

		for (i=0;i<ny;i++) {


			cell.ptlist[1].x = P.x;
			cell.ptlist[1].y = P.y;

			cell.ptlist[2].x = P.x + delta_qr_x;
			cell.ptlist[2].y = P.y + delta_qr_y;

			fudge_x_counter = fudge_x;
			inc_x = (fudge_x ? 1 : 0);

			/*	the coloumns	*/
			for (j=0;j<nx;j++, index++ ) {


				cell.ptlist[0].x = cell.ptlist[1].x; 
				cell.ptlist[0].y = cell.ptlist[1].y;

				cell.ptlist[3].x = cell.ptlist[2].x; 
				cell.ptlist[3].y = cell.ptlist[2].y;

				cell.ptlist[1].x += (delta_pr_x + inc_x);
				cell.ptlist[1].y += (delta_pr_y + inc_y);

				cell.ptlist[2].x += (delta_pr_x + inc_x);
				cell.ptlist[2].x += (delta_pr_y + inc_y);


				/* make sure data available in cgmc	*/
				if (index == c->Cnum && c->more) {
					if (Instr_Dec(c) < 1) {
						ct_error(T_FRE, "metafile");
						return (DIE);
					}

					index = 0;
				}

				fill_color((Cint) c->c[index]);

				polygon(&cell);

				/* see if we're still fudging	*/
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
	}

		return(OK);
}
