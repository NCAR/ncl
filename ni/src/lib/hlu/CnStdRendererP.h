/*
 *      $Id: CnStdRendererP.h,v 1.3 2008-01-05 01:18:22 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CnStdRendererP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	CnStdRenderer plot object private header file
 */

#ifndef _NCNSTDRENDERERERP_h
#define _NCNSTDRENDERERERP_h

#include <ncarg/hlu/CnStdRenderer.h>
#include <ncarg/hlu/CnRendererP.h>


typedef struct _NhlCnStdRendererLayerPart {
	NhlBoolean do_bounds;
} NhlCnStdRendererLayerPart;


typedef struct _NhlCnStdRendererLayerRec {
	NhlObjLayerPart			base;
	NhlCnRendererLayerPart  	cnrenderer;
	NhlCnStdRendererLayerPart	cnstdrenderer;
} NhlCnStdRendererLayerRec;


typedef struct NhlCnStdRendererClassPart{
	int foo;
} NhlCnStdRendererClassPart;


typedef struct _NhlCnStdRendererClassRec{
	NhlObjClassPart			base_class;
	NhlCnRendererClassPart		cnrenderer_class;
	NhlCnStdRendererClassPart	cnstdrenderer_class;
} NhlCnStdRendererClassRec;

typedef struct _NhlCnStdRendererClassRec	*NhlCnStdRendererClass;
typedef struct _NhlCnStdRendererLayerRec	*NhlCnStdRendererLayer;

extern NhlCnStdRendererClassRec	NhlcnStdRendererClassRec;

#endif  /* _NCNSTDRENDERERP_h */
