/*
 *	$Id: linesim.c,v 1.2 1991-01-09 11:10:54 clyne Exp $
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
#include	<cterror.h>
#include	<ncarv.h>
#include	<stdio.h>
#include	<math.h>
#include	"cgmc.h"
#include	"default.h"

/*	linesim.c:
 *
 *	author :	John Clyne
 *			4/25/88
 *
 *		
 *		This file contains the CGM line type simulation routine.
 */

extern 	char*	program_name;

/*	line_type:
 *		This function generates the set of points necessary to 
 *	implement a particular line type, as described by the CGM command
 *	"Line Type", for a **single** line.
 *
 *	on entry: 
 *		x1,y1 : coordinates for one end of the line
 *		x2,y2 : coordinates for other end of the line
 *		LINE_TYPE : is set in the default table to a designated 
 *			line type 2 through 5.
 *		XMAX, XMIN : are set in default table.
 *	on exit:
 *		line_type prints the set pairs of points implementing the type
 *
 *	Note:	If the line type is normal (LINE_TYPE = 1) this routine should
 *		NOT be invoked.
 */

Ct_err	line_sim (x1,y1,x2,y2)
	VDCtype	x1,y1,x2,y2;
{

	float	delta_x, delta_y;	/* increments along coordinate space
					 * needed to make dots and dashes
					 */
	float	len,			/* length of entire line */
		seglen,			/* length of dot or dash segment */
		x_space, y_space;	/* x,y coordinates for placements of
					 * dots and dashes
					 */
 
	int	inc,		/*number segment that will fit in the line*/
		i;

	VDCtype	xtemp, ytemp;



	boolean	dash = TRUE;	/*indicates dot or dash		*/

	seglen = (XMAX - XMIN) * 0.01;	/*seglen is 0.01*VDC extent	*/

	/*want to draw in positive x direction to simplify case statements	*/
	if(x2<x1) { 	xtemp = x1; x1 = x2; x2 = xtemp;
			ytemp = y1; y1 = y2; y2 = ytemp;
	}

	/* calculate length of line	*/
	len = sqrt((double)(((x2-x1)*(x2-x1)) + ((y2-y1)*(y2-y1))));

	/*calculate x and y coordinates for dot or dash		*/
	delta_x = seglen*(x2-x1)/len;
	delta_y = seglen*(y2-y1)/len;

	x_space = x1; y_space = y1;	/*begining of line	*/

	inc = len/seglen;

	switch (LINE_TYPE) {
		case 2:	/*line type is dash	*/
			for(i=0;i<inc/2;i++) {
				xtemp = (VDCtype)x_space;
				ytemp = (VDCtype)y_space;
				x_space += delta_x; y_space += delta_y;

				/*output the dash	*/
				line(xtemp,ytemp,(VDCtype)x_space,(VDCtype)y_space);
				x_space += delta_x; y_space += delta_y;
			}
			break;
		case 3:	/*line type is dot	*/
			dash = FALSE;
			for(i=0;i<inc;i++) {
				/*output the dot	*/
				line((VDCtype)x_space,(VDCtype)y_space,
					(VDCtype)x_space,(VDCtype)y_space);
				x_space += delta_x; y_space += delta_y;
			}
			break;
		case 4:	/*line type is dash dot	*/
			i=0;
			while(i < inc) {
				if (dash) {
					xtemp = (VDCtype)x_space;
					ytemp = (VDCtype)y_space;
					x_space += delta_x; y_space += delta_y;

					/*output the dash	*/
					line(xtemp,ytemp,
						(VDCtype)x_space,(VDCtype)y_space);
					x_space += delta_x; y_space += delta_y;
					dash = FALSE;
					i+=2;
				}
				else {
					/*output the dot	*/
					line((VDCtype)x_space,(VDCtype)y_space,
						(VDCtype)x_space,(VDCtype)y_space);
					x_space += delta_x; y_space += delta_y;
					dash = TRUE;
					i++;
				}
			}
			break;
					
		case 5: /*line type is dash dot	dot	*/
			for(i=0;i<inc/2;i++) {
				if (dash) {
					xtemp = (VDCtype)x_space;
					ytemp = (VDCtype)y_space;
					x_space += delta_x; y_space += delta_y;

					/*output the dash	*/
					line(xtemp,ytemp,
						(VDCtype)x_space,(VDCtype)y_space);
					x_space += delta_x; y_space += delta_y;
					dash = FALSE;
				}
				else {
					/*output the first dot	*/
					line((VDCtype)x_space,(VDCtype)y_space,
						(VDCtype)x_space,(VDCtype)y_space);
					x_space += delta_x; y_space += delta_y;

					/*output the second dot	*/
					line((VDCtype)x_space,(VDCtype)y_space,
						(VDCtype)x_space,(VDCtype)y_space);
					x_space += delta_x; y_space += delta_y;
					dash = TRUE;
				}
			}
			break;
		default:
			ct_error(NT_UPLS,"");
			return (SICK);
	}
	/* see if room for one more dash or dot as case may be	*/
	if (dash) {	/*we're printing dashed	*/
		if (((x_space+delta_x)< x2) && ((y_space+delta_x) < y2)) {
			line((VDCtype) x_space, (VDCtype) y_space, 
				(VDCtype) (x_space+delta_x), 
				(VDCtype) (y_space+delta_y));
		} 
		else
			line((VDCtype) x_space,(VDCtype) y_space, x2, y2);
	}
	else {		/*we're printing dots	*/

		if ((x_space < x2) && (y_space < y2)) {
			line((VDCtype) x_space, (VDCtype) y_space, 
				(VDCtype) (x_space), (VDCtype) (y_space));
		} 
		else
			line(x2,y2, x2, y2);
	}

	return (OK);
}
					
