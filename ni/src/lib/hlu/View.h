
/*
 *      $Id: View.h,v 1.1 1993-04-30 17:25:50 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		View.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 09:59:16 MDT 1992
 *
 *	Description:	Public header file for the ViewLayerClass.
 */
#ifndef _NView_h
#define _NView_h

#include <ncarg/hlu/Base.h>

#define NhlNvpXF "vpXF"
#define NhlCvpXF "VpXF"
#define NhlNvpYF "vpYF"
#define NhlCvpYF "VpYF"
#define NhlNvpWidthF "vpWidthF"
#define NhlCvpWidthF "VpWidthF"
#define NhlNvpHeightF "vpHeightF"
#define NhlCvpHeightF "VpHeightF"
#define NhlNvpKeepAspect	"vpKeepAspect"
#define NhlCvpKeepAspect	"VpKeepAspect"

typedef struct _ViewLayerClassRec *ViewLayerClass;
typedef struct _ViewLayerRec *ViewLayer;

extern LayerClass viewLayerClass;


#endif /*_NView_h*/
