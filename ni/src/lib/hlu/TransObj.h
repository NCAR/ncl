
/*
 *      $Id: TransObj.h,v 1.2 1993-05-27 19:11:27 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransObj.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 10:48:21 MDT 1992
 *
 *	Description:	This is the public header file for the TransObj class.
 *			The TransObjLayerClass is responsible for managing 
 *			transformations DATA==>VIEWPORT definitions.
 */
#ifndef _NTransObj_h
#define  _NTransObj_h

#include <ncarg/hlu/Base.h>


#define NhlNtrDashPattern	"trDashPattern"
#define NhlNtrLineLabel		"trLineLabel"
#define NhlNtrLineThicknessF	"trLineThicknessF"
#define NhlNtrLineLabelFontHeightF	"trLineLabelFontHeightF"
#define NhlNtrLineDashSegLenF	"trLineDashSegLenF"
#define NhlNtrLineColor		"trLineColor"

#define NhlCtrDashPattern	"TrDashPattern"
#define NhlCtrLineLabel		"TrLineLabel"
#define NhlCtrLineThicknessF	"TrLineThicknessF"
#define NhlCtrLineLabelFontHeightF	"TrLineLabelFontHeightF"
#define NhlCtrLineDashSegLenF	"TrLineDashSegLenF"
#define NhlCtrLineColor		"TrLineColor"

typedef struct _TransObjLayerClassRec *TransObjLayerClass;
typedef struct _TransObjLayerRec *TransObjLayer;

extern LayerClass transObjLayerClass;


#endif  /*_NTransObj_h*/
