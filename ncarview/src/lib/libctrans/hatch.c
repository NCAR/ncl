/*
 *	$Id: hatch.c,v 1.10 2008-07-27 03:18:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1992                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	hatch.c
 *
 *	Author		John Clyne
 * 
 *	Date		Fri Mar 29 12:48:07 MST 1991
 *
 *	simulate hatch patterns
 */
#include <stdio.h>
#include <math.h>
#include "default.h"
#include "soft_fill.h"
#include "translate.h"
#include "ctrandef.h"
#include "hatch.h"

/*
 *	C = A x B, where A,B,C are of type Matrix2d
 */
static	void	matrix_matrix_multiply(A, B, C)
	Matrix2d	A, B, C;
{
	int	i,j, k;

	for (i=0; i<3; i++)
	for (j=0; j<3; j++) {
		C[i][j] = 0;
		for (k=0; k<3; k++) {
			C[i][j] += A[i][k] * B[k][j];
		}
	}
		
}


/*
 *	A = I, where A is of type Matrix2d
 */
static	void	identity_matrix(A)
	Matrix2d	A;
{
	int	i,j;

	for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	{
		if (i == j)
			A[i][j] = 1.0;
		else
			A[i][j] = 0.0;
	}
}

/*
 * set up a composite matrix for performing the desired translation,
 * rotation and scaling
 */
static	set_up_matrix(xo, yo, cos_theta, sin_theta, scale, xprime, yprime, M)
	int	xo, yo;			/* translation to origin	*/
	double	cos_theta, 
		sin_theta;		/* angle of rotation	*/
	double	scale;
	int	xprime, yprime;		/* final translation destinatin	*/
	Matrix2d	M;
{
	Matrix2d	To,		/* translation to origin matrix	*/
			R,		/* rotation about origin	*/
			S,
			Tprime;		/* translation to pprime matrix	*/
	Matrix2d	Tmp;
	Matrix2d	Tmp1;

	identity_matrix(To);
	identity_matrix(R);
	identity_matrix(S);
	identity_matrix(Tprime);

	/*
	 * translate to origin
	 */
	To[0][2] = xo;
	To[1][2] = yo;


	/*
	 * rotate theta degrees about origin
	 */
	R[0][0] = cos_theta; R[0][1] = -sin_theta;
	R[1][0] = sin_theta; R[1][1] = cos_theta;

	/*
	 * scale
	 */
	S[0][0] = scale;
	S[1][1] = scale;

	/*
	 * translate to a point p'
	 */
	Tprime[0][2] = xprime;
	Tprime[1][2] = yprime;

	matrix_matrix_multiply(S, To, Tmp);
	matrix_matrix_multiply(R, Tmp, Tmp1);
	matrix_matrix_multiply(Tprime, Tmp1, M);
}

/*
 *	ComSimHatch
 *
 *	simulate the CGM hatch patterns in software. ComSimHatch() uses
 *	the solid fill software to build a table of horizontal lines which
 *	completely fill the polygon. To simulate hatch lines only a few of
 *	the line described by the fill table are used. If the hatch style
 *	is not horizontal the polygon is rotated before the fill table is
 *	is built because the fill table expects you to fill with horizontal
 *	lines.
 *
 * on entry
 *	p_list		: list of points describing the polygon
 *	n		: num elements in p_list
 *	hatch_index	: valid CGM hatch indece
 *	fill_scale_factor	: space between two horizontal lines on device
 *	dev		: describes the device
 * on exit
 *	p_list		: is undefined
 */
void	ComSimHatch(p_list, n, hatch_index, hatch_spacing, dev)
	Ptype	*p_list;
	long	n;
	IXtype	hatch_index;
	int	hatch_spacing;
	ComDev	*dev;
{
	FillTable       *fill_table;
	int     j,k;
	int	num_hatch;	/* some hatch patterns are cross hatches */
	DCtype	i;
	Matrix2d	M[2], 		/* matrix for rotating polygon	*/
			Minv[2];	/* matrix for restoring polygon	*/
	double		sin_theta[2],	/* cosine of angle of rotation	*/
			cos_theta[2];	/* sine of angle of rotation	*/
	double		sin_theta_inv[2],	/* cos of angle inverse	*/
			cos_theta_inv[2];	/* sin of angle inverse	*/
	double	x1, y1, x2, y2;
	double	x1_, y1_, x2_, y2_;
	int	x,y;
	DCtype	xmin, xmax,		/* x,y extent of polygon	*/
		ymin, ymax;
	DCtype	x_cen,			/* center of polygon		*/
		y_cen;
	DCtype	x_scr_cen,		/* center of screen		*/
		y_scr_cen;
	double	scale = 2.0;


	if (n < 2)
		return;
	/*
	 * we use lines to do the hatch patterns so we need to set
	 * line colour to the polygon colour. Mark line color as 
	 * having changed.
	 */
	dev->setlinecolour(FILL_COLOUR.index);/* set LINE color */
	LINE_COLOUR_DAMAGE = TRUE;

	/*
	 * convert VDC coordinates to DC (device) coordinates. Find the 
	 * x and y extents of polygon so we can determine its center.
	 * we overwrite the contents of p_list with device coordinates.
	 * (note this only legal since VDCtype and DCtype are the same type)
	 */
	xmin = XConvert(XMAX);	/* initialize extents	*/
	xmax = XConvert(XMIN);
	ymin = YConvert(YMAX);
	ymax = YConvert(YMIN);
	for (i = 0; i < n; i++) {
		p_list[i].x = XConvert(p_list[i].x);
		p_list[i].y = YConvert(p_list[i].y);

                if (xmax < p_list[i].x) xmax = (DCtype) p_list[i].x;
                if (ymax < p_list[i].y) ymax = (DCtype) p_list[i].y;
                if (xmin > p_list[i].x) xmin = (DCtype) p_list[i].x;
                if (ymin > p_list[i].y) ymin = (DCtype) p_list[i].y;

	}

	/*
	 * the center of the polygon
	 */
	x_cen = (xmin + xmax) / 2;
	y_cen = (ymin + ymax) / 2;

	/*
	 * the center of the screen
	 */
	x_scr_cen = (XConvert(XMAX) + XConvert(XMIN)) / 2;
	y_scr_cen = (YConvert(YMAX) + YConvert(YMIN)) / 2;

	/*
	 * determine the angle of the hatch pattern(s) and the number of 
	 * patterns, (1 or 2)
	 */ 
	num_hatch = 1;	/* only one pattern by default	*/
	switch (hatch_index) {

		case HORIZONTAL:	/* 0 degrees rotation	*/
			cos_theta[0] = 1.0; sin_theta[0] = 0.0;
			cos_theta_inv[0] = 1.0; sin_theta_inv[0] = 0.0;
			break;

		case VERTICAL  :	/* rotate 90 degrees	*/
			cos_theta[0] = 0.0; sin_theta[0] = 1.0;
			cos_theta_inv[0] = 0.0; sin_theta_inv[0] = -1.0;
			break;

		case POSITIVE :	/* rotate 45 degrees	*/
			cos_theta[0] = -0.707107; sin_theta[0] = 0.707107;
			cos_theta_inv[0] = sin_theta_inv[0]= -0.707107;
			break;

		case NEGATIVE  :	/* rotate 135 degrees	*/
			cos_theta[0] = 0.707107; sin_theta[0] = 0.707107;
			cos_theta_inv[0]= 0.707107; sin_theta_inv[0]= -0.707107;
			break;

		case HORIZ_VERT:
			cos_theta[0] = 1.0; sin_theta[0] = 0.0;
			cos_theta_inv[0] = 1.0; sin_theta_inv[0] = 0.0;

			cos_theta[1] = 0.0; sin_theta[1] = 1.0;
			cos_theta_inv[1] = 0.0; sin_theta_inv[1] = -1.0;
			num_hatch = 2;	/* two cross hatches	*/
			break;

		case POS_NEG  :
			cos_theta[0] = 0.707107; sin_theta[0] = 0.707107;
			cos_theta_inv[0]= 0.707107; sin_theta_inv[0]= -0.707107;
			cos_theta[1] = -0.707107; sin_theta[1] = 0.707107;
			cos_theta_inv[1] = sin_theta_inv[1]= -0.707107;
			num_hatch = 2;	/* two cross hatches	*/
			break;
		default:
			cos_theta[0] = 1.0; sin_theta[0] = 0.0;
			cos_theta_inv[0] = 1.0; sin_theta_inv[0] = 0.0;
			break;
	}

	/*
	 *	draw hatch pattern one angle at a time
	 */
	for (k = 0; k < num_hatch; k++) { 

		/*
		 * set up matrices for translating and rotating polygon. We
		 * rotate the polygon an angle theta because the software
		 * fill algorithm expects polygons to be filled using 
		 * horizontal lines. We have to translate to ensure that
		 * polygon remains inside the screen since software fill
		 * routine expect the polygon to be clipped to the screen
		 */
		set_up_matrix(-x_cen, -y_cen, cos_theta[k], sin_theta[k], 
				1.0/scale, x_scr_cen, y_scr_cen, M[k]);
		set_up_matrix(-x_scr_cen,-y_scr_cen, cos_theta_inv[k], 
				sin_theta_inv[k], scale, x_cen, y_cen, Minv[k]);

	
		/*
		 * translate and rotate polygon
		 */
		for (i=0; i < n; i++) {
			x = p_list[i].x; y = p_list[i].y;
			p_list[i].x = (x * M[k][0][0]) 
				+ (y * M[k][0][1]) + M[k][0][2]; 

			p_list[i].y = (x * M[k][1][0]) 
				+ (y * M[k][1][1]) + M[k][1][2]; 
		}

		/*
		 * Build the fill table. 
		 * This only works because p_list is of type Ptype 
		 * which happens  to be the same as DCpoint. 
		 */
		fill_table = buildFillTable(p_list, (unsigned) n);


		/*
		 * draw hatch lines between using even-odd fill rule
		 */
		for (i=fill_table->y_first; i < (fill_table->y_last + 1); i+=hatch_spacing) 
		for (j = 0; j < (fill_table->x_count[XC_INDEX(i)] - 1); j+=2) {

			x1_ = (double) fill_table->x_coord[XC_INDEX(i)][j];
			y1_ = (double) i;
			x2_ = (double) fill_table->x_coord[XC_INDEX(i)][j+1];
			y2_ = (double) i;

			x1 = (x1_ * Minv[k][0][0])+(y1_ * Minv[k][0][1]) 
								+ Minv[k][0][2];
			y1 = (x1_ * Minv[k][1][0])+(y1_ * Minv[k][1][1]) 
								+ Minv[k][1][2];
			x2 = (x2_ * Minv[k][0][0])+(y2_ * Minv[k][0][1]) 
								+ Minv[k][0][2];
			y2 = (x2_ * Minv[k][1][0])+(y2_ * Minv[k][1][1]) 
								+ Minv[k][1][2];

			dev->devline(ROUND(x1),ROUND(y1),ROUND(x2),ROUND(y2));
		}

		/*
		 * if we have to draw more hatch lines at a different theta
		 * restore point list to original values.
		 */
		if (k + 1 < num_hatch) {
			for (i=0; i < n; i++) {
				x = p_list[i].x; y = p_list[i].y;
				p_list[i].x = (x * Minv[k][0][0]) 
					+ (y * Minv[k][0][1]) + Minv[k][0][2];

				p_list[i].y = (x * Minv[k][1][0]) 
					+ (y * Minv[k][1][1]) + Minv[k][1][2];
			}
		}
	}
}
