/*
 *	$Id: gks_device.c,v 1.3 1994-06-08 16:57:32 boote Exp $
 */
/*
 *      File:		gks_device.c
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	This file contains the devices for the GKS driver. To 
 *			add a new device simply add the appropriate functions 
 *			to dev_tab[]. Each function  is expected to return an 
 *			int as status (-1 => error).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncarg/c.h>
#include "gks_device.h"


#ifdef	CTXT
#include "ctxt_device.h"
#include "ctxt.h"
#endif


#ifdef	X11
#include <X11/Xlib.h>
#include "x_device.h"
#endif

#ifdef	PS
#include "ps.h"
#include "ps_device.h"
#endif


int	Verbose = 0;


/*
 *	null - an unsupported function
 */
int	null()
{
	return(0);
}

/*
 *	the device table. Support for output devices is defined here. The
 *	format of this table is given in gks_device.h
 */
static	GKSdev	dev_tab[] = 	{

#ifdef	CTXT
	{
		"ctxt", 
		ctxt_ConvPoints, sizeof(CTXTPoint), 
		ctxt_ConvString, sizeof(char), ctxt_ConvInts, sizeof(int), 
		ctxt_ConvFloats, sizeof(int), ctxt_ConvIndexes, sizeof(int),
		ctxt_ConvRGBs, sizeof(CTXTColor),

		ctxt_OpenWorkstation, ctxt_ActivateWorkstation, 
		ctxt_CloseWorkstation, ctxt_ClearWorkstation, 
		ctxt_Polyline, ctxt_Polymarker, ctxt_Text, ctxt_FillArea, 
		ctxt_Cellarray, ctxt_SetLinetype, ctxt_SetLineWidthScaleFactor, 
		ctxt_SetPolylineColorIndex, ctxt_SetMarkerType, 
		ctxt_SetMarkerSizeScaleFactor, ctxt_SetPolymarkerColorIndex, 
		ctxt_SetTextFontAndPrecision, ctxt_SetCharacterExpansionFactor, 
		ctxt_SetCharacterSpacing, ctxt_SetTextColorIndex, 
		ctxt_SetCharacterHeightAndUpVector, ctxt_SetTextPath, 
		ctxt_SetTextAlignment, ctxt_SetFillAreaInteriorStyle, 
		ctxt_SetFillAreaStyleIndex, ctxt_SetFillAreaColorIndex, 
		ctxt_SetColorRepresentation, ctxt_SetClipIndicator, 
		ctxt_SetWindow, ctxt_GetColorRepresentation,
		ctxt_Esc, ctxt_UpdateWorkstation, ctxt_DeactivateWorkstation,
		ctxt_SetViewport
	},
#endif	/*	CTXT	*/

#ifdef	X11
	{
		"X11", 
		X11_ConvPoints,sizeof(XPoint), X11_ConvString,sizeof(char),
		X11_ConvInts, sizeof(int), X11_ConvFloats, sizeof (float),
		X11_ConvIndexes, sizeof (unsigned long), 
		X11_ConvRGBs,sizeof(XColor),

		X11_OpenWorkstation, X11_ActivateWorkstation, 
		X11_CloseWorkstation, X11_ClearWorkstation, X11_Polyline, 
		X11_Polymarker, X11_Text, X11_FillArea, X11_Cellarray, 
		X11_SetLinetype, X11_SetLineWidthScaleFactor, 
		X11_SetPolylineColorIndex, X11_SetMarkerType, 
		X11_SetMarkerSizeScaleFactor, X11_SetPolymarkerColorIndex, 
		X11_SetTextFontAndPrecision, X11_SetCharacterExpansionFactor, 
		X11_SetCharacterSpacing, X11_SetTextColorIndex, 
		X11_SetCharacterHeightAndUpVector, X11_SetTextPath, 
		X11_SetTextAlignment, X11_SetFillAreaInteriorStyle, 
		X11_SetFillAreaStyleIndex, X11_SetFillAreaColorIndex, 
		X11_SetColorRepresentation, X11_SetClipIndicator, X11_SetWindow,
		X11_GetColorRepresentation, X11_Esc, X11_UpdateWorkstation,
		X11_DeactivateWorkstation, X11_SetViewport
	},
#endif	/*	X11	*/

#ifdef	PS
	{
		"ps", 
		ps_ConvPoints, sizeof(CTXTPoint), 
		ps_ConvString, sizeof(char), ps_ConvInts, sizeof(int), 
		ps_ConvFloats, sizeof(int), ps_ConvIndexes, sizeof(int),
		ps_ConvRGBs, sizeof(CTXTColor),

		ps_OpenWorkstation, ps_ActivateWorkstation, 
		ps_CloseWorkstation, ps_ClearWorkstation, 
		ps_Polyline, ps_Polymarker, ps_Text, ps_FillArea, 
		ps_Cellarray, ps_SetLinetype, ps_SetLineWidthScaleFactor, 
		ps_SetPolylineColorIndex, ps_SetMarkerType, 
		ps_SetMarkerSizeScaleFactor, ps_SetPolymarkerColorIndex, 
		ps_SetTextFontAndPrecision, ps_SetCharacterExpansionFactor, 
		ps_SetCharacterSpacing, ps_SetTextColorIndex, 
		ps_SetCharacterHeightAndUpVector, ps_SetTextPath, 
		ps_SetTextAlignment, ps_SetFillAreaInteriorStyle, 
		ps_SetFillAreaStyleIndex, ps_SetFillAreaColorIndex, 
		ps_SetColorRepresentation, ps_SetClipIndicator, 
		ps_SetWindow, ps_GetColorRepresentation,
		ps_Esc, ps_UpdateWorkstation, ps_DeactivateWorkstation,
		ps_SetViewport
	},
#endif	/*	PS	*/
};


static	int	numDev = sizeof (dev_tab) / sizeof (dev_tab[0]);


/*
 *	GKS_GetDevByName
 *	[exported]
 *
 *	return a device named by 'name'
 * on entry
 *	*name		: name of gks output device
 * on exit
 *	return		: the appropriate GKSdev structure or NULL if device
 *			  could not be found.
 */
GKSdev	*GKS_GetDevByName(name)
	char	*name;
{
	GKSdev	*ptr;
	int	i;


	static	int	first = 1;

	if (first) {
		if (getenv("GKS_VERBOSE")) {
			Verbose = 1;
		}
		first = 0;
	}

	/*
	 * look for device 'name'
	 */
	for (i=0; i < numDev; i++) {
		ptr = &dev_tab[i];
		if (strcmp(name, ptr->name) == 0) {
			return(ptr);
		}
	}

	/*
	 * device not found
	 */
	ESprintf(ERR_OPN_DEV, "device not found (%s)", name);
	return ((GKSdev *) NULL);
}
