
/*
 *      $Id: IrregularTransObj.h,v 1.1 1993-04-30 17:22:17 boote Exp $
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

#define NhlNtrXCoordPoints	"trXCoordPoints"
#define NhlCtrXCoordPoints	"TrXCoordPoints"
#define NhlNtrXInterPoints	"trXInterPoints"
#define NhlCtrXInterPoints	"TrXInterPoints"
#define NhlNtrXNumPoints	"trXNumPoints"
#define NhlCtrXNumPoints	"TrXNumPoints"
#define NhlNtrXReverse		"trXReverse"
#define NhlCtrXReverse		"TrXReverse"
#define NhlNtrXTension		"trXTension"
#define NhlCtrXTension		"TrXTension"
#define NhlNtrXSamples		"trXSamples"
#define NhlCtrXSamples		"TrXSamples"
#define NhlNtrXUseLog		"trXUseLog"
#define NhlCtrXUseLog		"TrXUseLog"

#define NhlNtrYCoordPoints	"trYCoordPoints"
#define NhlCtrYCoordPoints	"TrYCoordPoints"
#define NhlNtrYInterPoints	"trYInterPoints"
#define NhlCtrYInterPoints	"TrYInterPoints"
#define NhlNtrYNumPoints	"trYNumPoints"
#define NhlCtrYNumPoints	"TrYNumPoints"
#define NhlNtrYReverse		"trYReverse"
#define NhlCtrYReverse		"TrYReverse"
#define NhlNtrYTension		"trYTension"
#define NhlCtrYTension		"TrYTension"
#define NhlNtrYSamples		"trYSamples"
#define NhlCtrYSamples		"TrYSamples"
#define NhlNtrYUseLog		"trYUseLog"
#define NhlCtrYUseLog		"TrYUseLog"




typedef struct _IrregularTransObjLayerClassRec *IrregularTransObjLayerClass;
typedef struct _IrregularTransObjLayerRec	*IrregularTransObjLayer;

extern LayerClass irregularTransObjLayerClass;


#endif /* _NIrregularTransObj_h */

