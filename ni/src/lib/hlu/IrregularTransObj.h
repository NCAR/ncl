/*
 *      $Id: IrregularTransObj.h,v 1.11 2003-09-10 21:29:54 dbrown Exp $
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


#define NhlNtrXAxisType		"trXAxisType"
#define NhlCtrXAxisType		"TrXAxisType"
#define NhlNtrXInterPoints	"trXInterPoints"
#define NhlCtrXInterPoints	"TrXInterPoints"
#define NhlNtrXTensionF		"trXTensionF"
#define NhlCtrXTensionF		"TrXTensionF"
#define NhlNtrXSamples		"trXSamples"
#define NhlCtrXSamples		"TrXSamples"

#define NhlNtrYAxisType		"trYAxisType"
#define NhlCtrYAxisType		"TrYAxisType"
#define NhlNtrYInterPoints	"trYInterPoints"
#define NhlCtrYInterPoints	"TrYInterPoints"
#define NhlNtrYTensionF		"trYTensionF"
#define NhlCtrYTensionF		"TrYTensionF"
#define NhlNtrYSamples		"trYSamples"
#define NhlCtrYSamples		"TrYSamples"


extern NhlClass NhlirregularTransObjClass;

#endif /* _NIrregularTransObj_h */

