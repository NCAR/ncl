
/*
 *      $Id: Transform.h,v 1.1 1993-04-30 17:25:29 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Transform.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 16:36:47 MDT 1992
 *
 *	Description:	Public header for Transform class.
 */

#ifndef _NTransform_h
#define _NTransform_h

#include <ncarg/hlu/View.h>

typedef struct _TransformLayerClassRec *TransformLayerClass;
typedef struct _TransformLayerRec *TransformLayer;

extern LayerClass transformLayerClass;


#endif /*_NTransform_h */
