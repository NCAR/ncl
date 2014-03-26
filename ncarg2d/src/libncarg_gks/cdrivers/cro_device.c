/*
 *      $Id: cro_device.c,v 1.1 2009-04-08 23:25:41 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2008                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/

/*
 *      File:           crodevice.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed Aug 27 13:30:40 MDT 2008
 *
 *      Description:    This file lists the data conversion functions
 *                      as well as all of the functions that will
 *                      be called by the cairo drivers.
 */
#include "gks_device.h"
#include "cro.h"
#include "cro_device.h"

static GKSdev crodev =
{
  "cro", 
  NULL,

/*
 *  Functions for converting raw input data to device data.
 *  These functions are defined in cro_conv.c . 
 */
  cro_ConvPoints,  sizeof(CROPoint) , 
  cro_ConvString,  sizeof(char)     , 
  cro_ConvInts,    sizeof(int)      , 
  cro_ConvFloats,  sizeof(float)    , 
  cro_ConvIndexes, sizeof(int)      ,
  cro_ConvRGBs,    sizeof(CROColor) ,

/*
 *  These are all the functions in the driver that will be
 *  called from GKS.  These functions are defined in cro.c
 */
  cro_OpenWorkstation,
  cro_ActivateWorkstation, 
  cro_CloseWorkstation,
  cro_ClearWorkstation, 
  cro_Polyline, 
  cro_Polymarker, 
  cro_Text, 
  cro_FillArea, 
  cro_Cellarray, 
  cro_SetLinetype,
  cro_SetLineWidthScaleFactor, 
  cro_SetPolylineColorIndex,
  cro_SetMarkerType, 
  cro_SetMarkerSizeScaleFactor,
  cro_SetPolymarkerColorIndex, 
  cro_SetTextFontAndPrecision,
  cro_SetCharacterExpansionFactor, 
  cro_SetCharacterSpacing, 
  cro_SetTextColorIndex, 
  cro_SetCharacterHeightAndUpVector, 
  cro_SetTextPath, 
  cro_SetTextAlignment, 
  cro_SetFillAreaInteriorStyle, 
  cro_SetFillAreaStyleIndex, 
  cro_SetFillAreaColorIndex, 
  cro_SetColorRepresentation, 
  cro_SetClipIndicator, 
  cro_SetWindow, 
  cro_GetColorRepresentation,
  cro_Esc, 
  cro_UpdateWorkstation, 
  cro_DeactivateWorkstation,
  cro_SetViewport
};

GKSdev *GKS_GetCROdev ()
{
  return &crodev;
}
