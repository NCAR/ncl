/*
 *      $Id: Annotation.h,v 1.2 1994-06-07 18:54:12 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Annotation.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 20 14:22:36 MDT 1994
 *
 *	Description:	Annotation public header file
 */
#ifndef _NAnnotation_h
#define _NAnnotation_h

#include <ncarg/hlu/Base.h>

#define NhlNanOn		"anOn"
#define NhlNanPlotId		"anPlotId"
#define NhlNanResizeNotify	"anResizeNotify"
#define NhlNanZone		"anZone"
#define NhlNanSide		"anSide"
#define NhlNanJust		"anJust"
#define NhlNanOrthogonalPosF	"anOrthogonalPosF"
#define NhlNanParallelPosF	"anParallelPosF"
#define NhlNanTrackData		"anTrackData"
#define NhlNanDataXF		"anDataXF"
#define NhlNanDataYF		"anDataYF"

#define NhlCanOn		"AnOn"
#define NhlCanPlotId		"AnPlotId"
#define NhlCanResizeNotify	"AnResizeNotify"
#define NhlCanZone		"AnZone"
#define NhlCanSide		"AnSide"
#define NhlCanJust		"AnJust"
#define NhlCanOrthogonalPosF	"AnOrthogonalPosF"
#define NhlCanParallelPosF	"AnParallelPosF"
#define NhlCanTrackData		"AnTrackData"
#define NhlCanDataXF		"AnDataXF"
#define NhlCanDataYF		"AnDataYF"

extern NhlLayerClass NhlannotationLayerClass;
#endif  /* _NAnnotation_h */
