/*
 *	$Id: cell.c,v 1.3 1991-03-12 14:47:40 clyne Exp $
 */
/*
 *	cell.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Jul  5 14:19:29 MDT 1990
 *
 *	Massage cell array data necessary for all devices
 */

#include <stdio.h>
#include <math.h>
#include "cgmc.h"
#include "ctrandef.h"

/*
 *	cell_prep
 *	[exported]
 *
 *	Perform interpolation of Cell array data necessitated by imperfect
 *	mapping of cell array cells onto device pixels
 * on entry
 *	nx		: number of columns in the cell array
 *	ny		: number of rows in the cell array
 *	*rows		: Has dimension ny
 *	*cols		: Has dimension nx
 * on exit
 *	*rows		: row[i] specifies number of pixels for a cell in row i
 *	*cols		: col[i] specifies number of pixels for a cell in col i
 */
cell_prep(P, Q, R, rows, cols, nx, ny)
	Ptype	P, Q, R;
	int	*rows, 
		*cols;
	unsigned	nx, ny;
{
	int	i;
	int	left, right;
	double	inc, tmp;

#if defined(CRAY) || (defined(u370) && defined(unix)) || defined(hpux)
#define	rint(X)	((int) X)
#else
	extern	double	rint();
#endif


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

	/*
	 * map cell array onto available pixels. Use current IEEE
	 * rounding rules to determine whether a cell boundry includes
	 * a boundry pixel or not. rows[i] and cols[j] contain
	 * the number of pixels that make up cell[i,j] 
	 */
	inc = (double) abs((int) (P.x - R.x)) / (double) nx;
	for( right = 0, tmp = 0.0,i = 0; i < nx; i++) {	/* map cols*/
		left = right;
		tmp += inc;
		right =  (int) rint(tmp);
		cols[i] = right - left;
	}

	inc = (double) abs((int) (Q.y - R.y)) / (double) ny;
	for( right = 0, tmp = 0.0,i = 0; i < ny; i++) {	/* map rows*/
		left = right;
		tmp += inc;
		right =  (int) rint(tmp);
		rows[i] = right - left;
	}


}


irregular_cell_prep (P, Q, R, nx, ny,
		delta_pr_x, delta_pr_y, 
		delta_qr_x, delta_qr_y, 
		fudge_x, fudge_y)

	Ptype	P, Q, R;
	unsigned	nx, ny;
	int	*delta_pr_x,	/* length of x vector for delta_pr	*/ 
		*delta_pr_y;	/* length of y vector for delta_pr	*/ 

	int	*delta_qr_x,	/* length of x vector for delta_qr	*/
		*delta_qr_y;	/* length of y vector for delta_qr	*/

	int	*fudge_x, *fudge_y;


{

	float	delta_pr,	/* length of segment of a cell along line pr */
		delta_qr;	/* length of segment of a cell along line qr */
	float	pr, qr;		/* length of lines pr and qr		*/

	/*
	 *	diagram of values
	 */

	/*
						    pr.
		              _______________________________________________Q
		             /	              /
		            /  	       	     /
	 		   /	   	    /
		     	  /   	     	   /
	  delta_qr_x.    /	          /
	         ______ /________________/__________
          	|      /   delta_pr^    /
	  	|     /		       /
	  	|    /		      /
    delta_qr_y> |   / < delta_qr     /
	  	|  /		    /
	  	| /	           /
	  	|/________________/________________________________
	  	P						   R
					pr^
	*/


	/*	calculate lengths of cell array boundries	*/
	pr = (float) sqrt((double) (SQR(P.y - R.y) + SQR(P.x - R.x)));
	qr = (float) sqrt((double) (SQR(Q.y - R.y) + SQR(Q.x - R.x)));

	/*	calculate fudge factor			*/
	*fudge_x = (int) pr % nx;
	*fudge_y = (int) qr % ny;

	/*	calculate length of individual cell boundries	*/
	delta_pr = pr / nx;
	delta_qr = qr / ny;

	/* calculate lengths of vectors describing a cell boundry*/
	*delta_pr_x  = (int) ((delta_pr * (R.x - P.x)) / pr);
	*delta_pr_y  = (int) ((delta_pr * (R.y - P.y)) / pr);

	*delta_qr_x  = (int) ((delta_qr * (Q.x - R.x)) / qr);
	*delta_qr_y  = (int) ((delta_qr * (Q.y - R.y)) / qr);

}
