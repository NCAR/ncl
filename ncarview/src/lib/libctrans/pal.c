/*
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
		
#include <ncarg_ras.h>
#include <ncarv.h>
#include <cterror.h>
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

	char	*RasterGetError();

	/*
	 * The cgmc will contain the CGM color table command
	 */
	cgmc->class = ATT_ELEMENT;
	cgmc->command = COLOR_TABLE;

	/*
	 * maximum color representable by the CGM
	 */
	max_color = (1 << DCP) - 1;

	colors = (unsigned char *) icMalloc (768);

	/*
	 * make sure enough room in cgmc for data
	 */
	if (cgmc->CDspace < 256) {
		if (cgmc->CDspace) cfree((char *) cgmc->cd);
		cgmc->cd = (CDtype *) icMalloc(256 * sizeof (CDtype));
		cgmc->CDspace = 256;
	}


	/*
	 * read in the color palette
	 */
	if (PaletteRead(palette, NULL, colors) != RAS_OK) {
		ct_error(NT_NULL, RasterGetError());
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

	free((char *) colors);

	return(1);
}
