/*
 *      $Id: LogLinPlot.h,v 1.1 1993-11-20 01:06:08 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LogLinPlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for LogLinPlot class.
 */

#ifndef _NLogLinPlot_h
#define _NLogLinPlot_h

#include <ncarg/hlu/Overlay.h>

/*
 * LogLinPlot instance resources
 */
#define NhlNllOverlayPlotBase	"llOverlayPlotBase"

#define NhlNllXMinF		"llXMinF"
#define NhlNllXMaxF		"llXMaxF"
#define NhlNllXLog		"llXLog"
#define NhlNllXReverse		"llXReverse"

#define NhlNllYMinF		"llYMinF"
#define NhlNllYMaxF		"llYMaxF"
#define NhlNllYLog		"llYLog"
#define NhlNllYReverse		"llYReverse"

/*
 * LogLinPlot class resources
 */

#define NhlCllOverlayPlotBase	"LlOverlayPlotBase"

#define NhlCllXMinF		"LlXMinF"
#define NhlCllXMaxF		"LlXMaxF"
#define NhlCllXLog		"LlXLog"
#define NhlCllXReverse		"LlXReverse"

#define NhlCllYMinF		"LlYMinF"
#define NhlCllYMaxF		"LlYMaxF"
#define NhlCllYLog		"LlYLog"
#define NhlCllYReverse		"LlYReverse"

typedef struct _LogLinPlotLayerClassRec	*LogLinPlotLayerClass;
typedef struct _LogLinPlotLayerRec	*LogLinPlotLayer;

extern LayerClass logLinPlotLayerClass;

#endif /*_NLogLinPlot_h */
