/*
 *      $Id: pdfdevice.c,v 1.1 2003-01-06 23:30:15 fred Exp $
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
*                                                                       *
*                            Copyright (C)  1996                        *
*            University Corporation for Atmospheric Research            *
*                            All Rights Reserved                        *
*                                                                       *
************************************************************************/
/*
 *      File:           pdfdevice.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Tue Oct 15 18:04:15 MDT 2002
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
#include "pdf.h"
#include "pdf_device.h"

static GKSdev pdfdev =
{
        "pdf", 

        NULL,

        PDFConvPoints, sizeof(PDFPoint), 
        PDFConvString, sizeof(char), PDFConvInts, sizeof(int), 
        PDFConvFloats, sizeof(int), PDFConvIndexes, sizeof(int),
        PDFConvRGBs, sizeof(PDFColor),

        PDFOpenWorkstation, PDFActivateWorkstation, 
        PDFCloseWorkstation, PDFClearWorkstation, 
        PDFPolyline, PDFPolymarker, PDFText, PDFFillArea, 
        PDFCellarray, PDFSetLinetype, PDFSetLineWidthScaleFactor, 
        PDFSetPolylineColorIndex, PDFSetMarkerType, 
        PDFSetMarkerSizeScaleFactor, PDFSetPolymarkerColorIndex, 
        PDFSetTextFontAndPrecision, PDFSetCharacterExpansionFactor, 
        PDFSetCharacterSpacing, PDFSetTextColorIndex, 
        PDFSetCharacterHeightAndUpVector, PDFSetTextPath, 
        PDFSetTextAlignment, PDFSetFillAreaInteriorStyle, 
        PDFSetFillAreaStyleIndex, PDFSetFillAreaColorIndex, 
        PDFSetColorRepresentation, PDFSetClipIndicator, 
        PDFSetWindow, PDFGetColorRepresentation,
        PDFEsc, PDFUpdateWorkstation, PDFDeactivateWorkstation,
        PDFSetViewport
};

GKSdev
*GKS_GetPDFdev
#ifdef  NeedFuncProto
(
        void
)
#else
()
#endif
{
        return &pdfdev;
}
