/*
 *      $Id: Base.h,v 1.2 1993-10-19 17:49:41 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Base.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:36:51 MDT 1992
 *
 *	Description:	This file contains the external declarations neccessary
 *			to create an instance of the BaseClass layer.
 */
#ifndef _NBase_h
#define _NBase_h

typedef struct _ObjLayerRec *ObjLayer;
typedef struct _ObjLayerClassRec *ObjLayerClass;

typedef struct _BaseLayerRec *BaseLayer;
typedef struct _BaseLayerClassRec *BaseLayerClass;

extern LayerClass objLayerClass;
extern LayerClass baseLayerClass;
#endif  /* _NBase_h */
