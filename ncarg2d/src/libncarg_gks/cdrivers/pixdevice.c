/*
 *      $Id: pixdevice.c,v 1.2 2008-07-23 17:28:01 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2004                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*                                                                       *
*                            Copyright (C)  2004                        *
*            University Corporation for Atmospheric Research            *
*                            All Rights Reserved                        *
*                                                                       *
************************************************************************/
/*
 *      File:           pixdevice.c
 *
 *      Author:         Jeff W. Boote (original)
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Mar 12 15:25:59 MST 2004
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
#include <X11/Xlib.h>
#include "pix_device.h"

static GKSdev pixdev =
{
        "PIX", 

        PIX_Exec,

        PIX_ConvPoints,sizeof(XPoint), PIX_ConvString,sizeof(char),
        PIX_ConvInts, sizeof(int), PIX_ConvFloats, sizeof (float),
        PIX_ConvIndexes, sizeof (unsigned long), 
        PIX_ConvRGBs,sizeof(XColor),

        PIX_OpenWorkstation, PIX_ActivateWorkstation, 
        PIX_CloseWorkstation, PIX_ClearWorkstation, PIX_Polyline, 
        PIX_Polymarker, PIX_Text, PIX_FillArea, PIX_Cellarray, 
        PIX_SetLinetype, PIX_SetLineWidthScaleFactor, 
        PIX_SetPolylineColorIndex, PIX_SetMarkerType, 
        PIX_SetMarkerSizeScaleFactor, PIX_SetPolymarkerColorIndex, 
        PIX_SetTextFontAndPrecision, PIX_SetCharacterExpansionFactor, 
        PIX_SetCharacterSpacing, PIX_SetTextColorIndex, 
        PIX_SetCharacterHeightAndUpVector, PIX_SetTextPath, 
        PIX_SetTextAlignment, PIX_SetFillAreaInteriorStyle, 
        PIX_SetFillAreaStyleIndex, PIX_SetFillAreaColorIndex, 
        PIX_SetColorRepresentation, PIX_SetClipIndicator, PIX_SetWindow,
        PIX_GetColorRepresentation, PIX_Esc, PIX_UpdateWorkstation,
        PIX_DeactivateWorkstation, PIX_SetViewport
};

GKSdev
*GKS_GetPIXdev
#ifdef  NeedFuncProto
(
        void
)
#else
()
#endif
{
        return &pixdev;
}
