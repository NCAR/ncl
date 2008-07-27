/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 * $Id: pal.c,v 1.10 2008-07-27 03:18:44 haley Exp $
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
