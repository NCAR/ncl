/*
 *      $Id: VecAnno.h,v 1.4 1999-03-27 00:44:58 dbrown Exp $
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

typedef struct __NhlvaArrowParams {
	float ast_iast;
	float uvmx;
	float uvmn;
	float dvmx;
	float fw2w;
	float uvmg;
	float vlc_vlom;
	float vhc_vhim;
	float vfr_vfrc;
	float vrl_vrln;
        float vrm_vrmg;
	float amn_famn;
	float amx_famx;
	float air_fair;
	float awr_fawr;
	float awf_fawf;
	float axr_faxr;
	float axr_faxf;
	float ayr_fayr;
	float ayf_fayf;
	float afo_iafo;
        float wba_wbad;
        float wbt_wbtf;
        float wbc_wbcf;
        float wbd_wbdf;
        float wbs_wbsc;
} _NhlvaArrowParams;

typedef struct __NhlvaDrawParams {
	float xvpl;
        float xvpr;
        float xvpb;
        float xvpt;
        float wxmn;
        float wxmx;
        float wymn;
        float wymx;
        float invx;
        float invy;
        float lnlg;
} _NhlvaDrawParams;
        
/*
 * Resource name definitions
 */

#define NhlNvaString1		"vaString1"
#define NhlNvaString1On		"vaString1On"
#define NhlNvaString2		"vaString2"
#define NhlNvaString2On		"vaString2On"
#define NhlNvaVectorLenF	"vaVectorLenF"
#define NhlNvaVectorLineColor	"vaVectorLineColor"
#define NhlNvaVectorFillColor	"vaVectorFillColor"
#define NhlNvaArrowLineThicknessF	"vaArrowLineThicknessF"
#define NhlNvaArrowAngleF		"vaArrowAngleF"
#define NhlNvaArrowSpaceF		"vaArrowSpaceF"
#define NhlNvaArrowMinOffsetF		"vaArrowMinOffsetF"
#define NhlNvaArrowParams		"vaArrowParams"
#define NhlNvaDrawParams		"vaDrawParams"

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
#define NhlCvaArrowAngleF		"VaArrowAngleF"
#define NhlCvaArrowSpaceF		"VaArrowSpaceF"
#define NhlCvaArrowMinOffsetF		"VaArrowMinOffsetF"
#define NhlCvaArrowParams		"VaArrowParams"
#define NhlCvaDrawParams		"VaDrawParams"

#define NhlCvaPerimOn		"VaPerimOn"
#define NhlCvaPerimSpaceF	"VaPerimSpaceF"


/*
 * These class resources have been eliminated
 */

#if 0
#define NhlCvaPerimColor	"VaPerimColor"
#define NhlCvaPerimThicknessF	"VaPerimThicknessF"
#define NhlCvaBackgroundFillColor	"VaBackgroundFillColor"
#define NhlCvaVectorLineColor	"VaVectorLineColor"
#define NhlCvaVectorFillColor	"VaVectorFillColor"
#define NhlCvaArrowLineThicknessF	"VaArrowLineThicknessF"
#endif

/*
 * Definition and declaration of new class for global use
 */

extern NhlClass NhlvecAnnoClass;

#endif  /* _NVecAnno_h */
