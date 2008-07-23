/*
 *      $Id: xdevice.c,v 1.6 2008-07-23 17:28:02 haley Exp $
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
 *      File:           xdevice.c
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
#include <X11/Xlib.h>
#include "x_device.h"

static GKSdev xdev =
{
        "X11", 

        X11_Exec,

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
};

GKSdev
*GKS_GetX11dev
#ifdef  NeedFuncProto
(
        void
)
#else
()
#endif
{
        return &xdev;
}
