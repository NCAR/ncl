/*
 *      $Id: ctxt_device.c,v 1.6 2008-07-23 17:28:00 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*                                                                       *
*                            Copyright (C)  1996                        *
*            University Corporation for Atmospheric Research            *
*                            All Rights Reserved                        *
*                                                                       *
************************************************************************/
/*
 *      File:           ctxt_device.c
 *
 *      Author:         Jeff W. Boote
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Oct 18 09:17:29 MDT 1996
 *
 *      Description:    Breaking up gks_device.c so each driver can be a
 *                      delay-loaded dynamically shared object.
 *                      (The old dev_tab caused the text segments of all
 *                      the cdrivers to be included in the data segment -
 *                      and therefore caused resolution of those symbols
 *                      from the dso's even though the text symbols were
 *                      not actually called.  This way forces a symbol
 *                      call to actually force the dso to load.
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
#ifdef  NeedFuncProto
(
        void
)
#else
()
#endif
{
        return &ctxtdev;
}
