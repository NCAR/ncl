
/*
 *      $Id: IrregularPlot.h,v 1.2 1993-12-22 00:55:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularPlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for IrregularPlot class.
 */

#ifndef _NIrregularPlot_h
#define _NIrregularPlot_h

#include <ncarg/hlu/Overlay.h>

typedef struct _IrregularPlotLayerClassRec *IrregularPlotLayerClass;
typedef struct _IrregularPlotLayerRec *IrregularPlotLayer;

extern LayerClass irregularPlotLayerClass;

#endif /*_NIrregularPlot_h */
