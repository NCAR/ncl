/*
 *      $Id: VecAnno.h,v 1.1 1995-11-21 20:19:03 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VecAnno.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct  9 19:11:27 MDT 1995
 *
 *	Description:	Public header file - for VecAnno item.
 */
#ifndef _NVecAnno_h
#define _NVecAnno_h

#include <ncarg/hlu/View.h>
#include <ncarg/hlu/TextItem.h>

/*
 * Resource name definitions
 */

#define NhlNvaString1		"vaString1"
#define NhlNvaString1On		"vaString1On"
#define NhlNvaString2		"vaString2"
#define NhlNvaString2On		"vaString2On"
#define NhlNvaVectorLenF	"vaVectorLenF"
#define NhlNvaVectorColor	"vaVectorColor"
#define NhlNvaArrowHeadMinSizeF		"vaArrowHeadMinSizeF"
#define NhlNvaArrowHeadMaxSizeF		"vaArrowHeadMaxSizeF"
#define NhlNvaArrowLineThicknessF	"vaArrowLineThicknessF"
#define NhlNvaArrowAngleF		"vaArrowAngleF"
#define NhlNvaArrowSpaceF		"vaArrowSpaceF"
#define NhlNvaArrowMinOffsetF		"vaArrowMinOffsetF"

#define NhlNvaPerimOn		"vaPerimOn"
#define NhlNvaPerimColor	"vaPerimColor"
#define NhlNvaPerimThicknessF	"vaPerimThicknessF"
#define NhlNvaPerimSpaceF	"vaPerimSpaceF"
#define NhlNvaBackgroundFillColor	"vaBackgroundFillColor"

#define NhlCvaString1		"VaString1"
#define NhlCvaString1On		"VaString1On"
#define NhlCvaString2		"VaString2"
#define NhlCvaString2On		"VaString2On"
#define NhlCvaVectorLenF	"VaVectorLenF"
#define NhlCvaVectorColor	"VaVectorColor"
#define NhlCvaArrowHeadMinSizeF		"VaArrowHeadMinSizeF"
#define NhlCvaArrowHeadMaxSizeF		"VaArrowHeadMaxSizeF"
#define NhlCvaArrowLineThicknessF	"VaArrowLineThicknessF"
#define NhlCvaArrowAngleF		"VaArrowAngleF"
#define NhlCvaArrowSpaceF		"VaArrowSpaceF"
#define NhlCvaArrowMinOffsetF		"VaArrowMinOffsetF"

#define NhlCvaPerimOn		"VaPerimOn"
#define NhlCvaPerimColor	"VaPerimColor"
#define NhlCvaPerimThicknessF	"VaPerimThicknessF"
#define NhlCvaPerimSpaceF	"VaPerimSpaceF"
#define NhlCvaBackgroundFillColor	"VaBackgroundFillColor"


/*
 * Definition and declaration of new class for global use
 */

extern NhlClass NhlvecAnnoClass;

#endif  /* _NVecAnno_h */
