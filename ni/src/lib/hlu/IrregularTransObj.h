
/*
 *      $Id: IrregularTransObj.h,v 1.2 1993-11-10 01:19:10 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularTransObj.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 14:49:18 MDT 1992
 *
 *	Description:	
 */
#ifndef _NIrregularTransObj_h
#define _NIrregularTransObj_h

#include <ncarg/hlu/TransObj.h>

#define NhlNtrXMinF     "trXMinF"
#define NhlCtrXMinF     "TrXMinF"
#define NhlNtrXMaxF     "trXMaxF"
#define NhlCtrXMaxF     "TrXMaxF"

#define NhlNtrXCoordPoints	"trXCoordPoints"
#define NhlCtrXCoordPoints	"TrXCoordPoints"
#define NhlNtrXInterPoints	"trXInterPoints"
#define NhlCtrXInterPoints	"TrXInterPoints"
#define NhlNtrXNumPoints	"trXNumPoints"
#define NhlCtrXNumPoints	"TrXNumPoints"
#define NhlNtrXReverse		"trXReverse"
#define NhlCtrXReverse		"TrXReverse"
#define NhlNtrXTensionF		"trXTensionF"
#define NhlCtrXTensionF		"TrXTensionF"
#define NhlNtrXSamples		"trXSamples"
#define NhlCtrXSamples		"TrXSamples"
#define NhlNtrXUseLog		"trXUseLog"
#define NhlCtrXUseLog		"TrXUseLog"

#define NhlNtrYMinF     "trYMinF"
#define NhlCtrYMinF     "TrYMinF"
#define NhlNtrYMaxF     "trYMaxF"
#define NhlCtrYMaxF     "TrYMaxF"
#define NhlNtrYCoordPoints	"trYCoordPoints"
#define NhlCtrYCoordPoints	"TrYCoordPoints"
#define NhlNtrYInterPoints	"trYInterPoints"
#define NhlCtrYInterPoints	"TrYInterPoints"
#define NhlNtrYNumPoints	"trYNumPoints"
#define NhlCtrYNumPoints	"TrYNumPoints"
#define NhlNtrYReverse		"trYReverse"
#define NhlCtrYReverse		"TrYReverse"
#define NhlNtrYTensionF		"trYTensionF"
#define NhlCtrYTensionF		"TrYTensionF"
#define NhlNtrYSamples		"trYSamples"
#define NhlCtrYSamples		"TrYSamples"
#define NhlNtrYUseLog		"trYUseLog"
#define NhlCtrYUseLog		"TrYUseLog"




typedef struct _IrregularTransObjLayerClassRec *IrregularTransObjLayerClass;
typedef struct _IrregularTransObjLayerRec	*IrregularTransObjLayer;

extern LayerClass irregularTransObjLayerClass;


#endif /* _NIrregularTransObj_h */

