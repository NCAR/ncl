/*
 *      $Id: ctxt_device.c,v 1.4 2000-08-22 15:09:48 haley Exp $
 */
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

/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ctxt_device.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 18 09:17:29 MDT 1996
 *
 *	Description:	Breaking up gks_device.c so each driver can be a
 *			delay-loaded dynamically shared object.
 *			(The old dev_tab caused the text segments of all
 *			the cdrivers to be included in the data segment -
 *			and therefore caused resolution of those symbols
 *			from the dso's even though the text symbols were
 *			not actually called.  This way forces a symbol
 *			call to actually force the dso to load.
 */
#include "gks_device.h"
#include "ctxt_device.h"
#include "ctxt.h"

static GKSdev ctxtdev =
{
	"ctxt", 

	NULL,

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
};

GKSdev
*GKS_GetCTXTdev
#ifdef	NeedFuncProto
(
	void
)
#else
()
#endif
{
	return &ctxtdev;
}
