/*
 *	$Id: rgbtohls.c,v 1.4 2000-07-12 18:00:50 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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
 *	Author:	Tinsley Galyean (tag@boulder.colorado.edu)
 *
 *	Date:	Thu Apr 20 15:15:44 MST 1988
 *
 *	This algorithm as taken form Foley and VanDam
 */

/*
 *	Recieves a value between [0,255] for red, green, and blue.
 *
 *	Returns a hue between [0,360)
 *		a lightness and saturation between [0,100]
 */
RGBtoHLS (red, green, blue, hue, lightness, saturation)
long	red,green,blue;
long	*hue,*lightness,*saturation;
{
	float	max,min;
	float	rc,gc,bc;

	float	r,g,b;
	float	h,l,s;

	r = (float)red / 255.0;
	g = (float)green / 255.0;
	b = (float)blue / 255.0;

	max = r > g ? ( r > b ? r : b ) : ( g > b ? g : b);
	min = r < g ? ( r < b ? r : b ) : ( g < b ? g : b);

	/*
	 *	Lightness 
	 */
	l = (max + min) / 2.0;

	/*
	 * 	Saturation
	 */
	if (max == min) {
		s = 0.0;
		h = 0.0;	/* undefined but am setting to 0 for now */
	} else {
		if (l < 0.5) 
			s = (max - min)/(max + min);
		else
			s = (max - min)/(2.0 - max - min);

		/*
		 *	Hue
		 */
		rc = (max - r)/(max - min);
		gc = (max - g)/(max - min);
		bc = (max - b)/(max - min);

		if (r == max)
			h = 2.0 + bc - gc;
		else if (g == max)
			h = 4.0 + rc - bc;
		else if (b == max)
			h = gc - rc;

		h = h * 60.0;
		if (h < 0.0)
			h += 360.0;
	}

	*hue = (int)h;
	*lightness = (int) (l * 100.0);
	*saturation = (int) (s * 100.0);
}
