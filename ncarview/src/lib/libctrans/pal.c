/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 * $Id: pal.c,v 1.9 2000-08-22 15:11:50 haley Exp $
 *	File:	pal.c
 *
 *	Author:	John Clyne
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	Thu Apr 18 13:14:55 MDT 1991
 *
 *	Description: 
 *		LoadPalette() is used to load a cgmc with a COLOR TABLE 
 *		ELEMENT whose palette is obtained from a palette file.
 */
		
#include <stdlib.h>
#include <errno.h>
#include <ncarg/c.h>
#include <ncarg/ncarg_ras.h>
#include "cgmc.h"
#include "default.h"

/*
 *	LoadPalette()
 *	[exported]
 *
 *	Load a palette into a cgmc. The palette is obtained from the 
 *	specified file. 
 *
 * on entry
 *	*palette	: name of file containing palette
 * on exit
 *	*cgmc		: contains a CGM COLOR TABLE element with the palette
 *			  contained in file palette
 */
LoadPalette(cgmc, palette)
	CGMC	*cgmc;
	char	*palette;
{
	unsigned char	*colors;
	int	i;
	long	max_color;

	/*
	 * The cgmc will contain the CGM color table command
	 */
	cgmc->cgmclass = ATT_ELEMENT;
	cgmc->command = COLOR_TABLE_ID;

	/*
	 * maximum color representable by the CGM
	 */
	max_color = (1 << DCP) - 1;

	if (! (colors = (unsigned char *) malloc (768))) {
		ESprintf(errno, "malloc(%d)", 768);
		return(-1);
	}

	/*
	 * make sure enough room in cgmc for data
	 */
	if (cgmc->CDspace < 256) {
		if (cgmc->CDspace) free((Voidptr) cgmc->cd);
		cgmc->cd = (CDtype *) malloc(256 * sizeof (CDtype));
		if (! cgmc->cd) {
			ESprintf(errno, "malloc(%d)", 256 * sizeof(CDtype));
			return(-1);
		}
		cgmc->CDspace = 256;
	}


	/*
	 * read in the color palette
	 */
	if (PaletteRead(palette, NULL, colors) != RAS_OK) {
		return(-1);
	}


	cgmc->ci[0] = 0;	/* starting color index	*/
	cgmc->CInum = 1;

	/*
	 * load the cgmc with the new palette, converting to appropriate
	 * color precision for this CGM
	 */
	for (i=0; i < 256; i++) {
		cgmc->cd[i].red = (unsigned long)
			((float) colors[i] / (float) 255 * (float) max_color);

		cgmc->cd[i].green = (unsigned long)
			((float) colors[i+256] / (float)255 * (float)max_color);

		cgmc->cd[i].blue = (unsigned long)
			((float) colors[i+512] / (float)255 * (float)max_color);
	}

	cgmc->CDnum = 256;	/* number of values defined	*/

	free((Voidptr) colors);

	return(1);
}
