
/*
 *      $Id: TransObj.h,v 1.3 1993-10-19 17:52:54 boote Exp $
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



typedef struct _TransObjLayerClassRec *TransObjLayerClass;
typedef struct _TransObjLayerRec *TransObjLayer;

extern LayerClass transObjLayerClass;


#endif  /*_NTransObj_h*/
