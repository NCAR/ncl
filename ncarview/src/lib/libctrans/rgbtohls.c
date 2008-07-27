/*
 *	$Id: rgbtohls.c,v 1.6 2008-07-27 03:18:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
