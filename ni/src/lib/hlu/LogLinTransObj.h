
/*
 *      $Id: LogLinTransObj.h,v 1.1 1993-04-30 17:22:44 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Nov 4 16:31:38 MST 1992
 *
 *	Description:	
 */
#ifndef _NLogLinTransObj_h
#define _NLogLinTransObj_h

#include <ncarg/hlu/TransObj.h>

#define NhlNtrXMinF	"trXMinF"
#define NhlCtrXMinF	"TrXMinF"
#define NhlNtrXMaxF	"trXMaxF"
#define NhlCtrXMaxF	"TrXMaxF"
#define NhlNtrXLog	"trXLog"
#define NhlCtrXLog	"TrXLog"
#define NhlNtrXReverse	"trXReverse"
#define NhlCtrXReverse	"TrXReverse"


#define NhlNtrYMinF	"trYMinF"
#define NhlCtrYMinF	"TrYMinF"
#define NhlNtrYMaxF	"trYMaxF"
#define NhlCtrYMaxF	"TrYMaxF"
#define NhlNtrYLog	"trYLog"
#define NhlCtrYLog	"TrYLog"
#define NhlNtrYReverse	"trYReverse"
#define NhlCtrYReverse	"TrYReverse"


typedef struct _LogLinTransObjLayerClassRec *LogLinTransObjLayerClass;
typedef struct _LogLinTransObjLayerRec	*LogLinTransObjLayer;

extern LayerClass logLinTransObjLayerClass;


#endif /* LogLinTransObj_h */

