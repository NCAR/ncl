/*
 *      $Id: AnnoManager.h,v 1.1 1995-04-01 00:03:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AnnoManager.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 20 14:22:36 MDT 1994
 *
 *	Description:	AnnoManager public header file
 */
#ifndef _NANNOMANAGER_h
#define _NANNOMANAGER_h

#include <ncarg/hlu/Base.h>

#define NhlNamOn		"amOn"
#define NhlNamViewId		"amViewId"
#define NhlNamResizeNotify	"amResizeNotify"
#define NhlNamZone		"amZone"
#define NhlNamSide		"amSide"
#define NhlNamJust		"amJust"
#define NhlNamOrthogonalPosF	"amOrthogonalPosF"
#define NhlNamParallelPosF	"amParallelPosF"
#define NhlNamTrackData		"amTrackData"
#define NhlNamDataXF		"amDataXF"
#define NhlNamDataYF		"amDataYF"

#define NhlCamOn		"AmOn"
#define NhlCamViewId		"AmViewId"
#define NhlCamResizeNotify	"AmResizeNotify"
#define NhlCamZone		"AmZone"
#define NhlCamSide		"AmSide"
#define NhlCamJust		"AmJust"
#define NhlCamOrthogonalPosF	"AmOrthogonalPosF"
#define NhlCamParallelPosF	"AmParallelPosF"
#define NhlCamTrackData		"AmTrackData"
#define NhlCamDataXF		"AmDataXF"
#define NhlCamDataYF		"AmDataYF"

extern NhlLayerClass NhlannoManagerLayerClass;
#endif  /* _NANNOMANAGER_h */
