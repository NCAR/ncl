
/*
 *      $Id: TransObjP.h,v 1.1 1993-04-30 17:25:22 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransObjP.h
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
#ifndef _NTransObjP_h
#define  _NTransObjP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/TransObj.h>


typedef struct _TransObjLayerPart {
/* Publicly setable resources */
	int foo;
/*
* Dummy variable
*/
	
}TransObjLayerPart;


typedef struct _TransObjLayerRec {
	BaseLayerPart	base;
	TransObjLayerPart	trobj;
}TransObjLayerRec;

typedef struct _TransObjLayerClassPart {
	NhlErrorTypes	(*set_trans)();
	NhlErrorTypes	(*trans_type)();
/*
* linear portion
*/
	NhlErrorTypes	(*win_to_ndc)();
	NhlErrorTypes	(*ndc_to_win)();
/*
* possibly not linear transformation
*/
	NhlErrorTypes	(*data_to_win)();
	NhlErrorTypes	(*win_to_data)();
/*
* intermediate transformations
*/
	NhlErrorTypes	(*data_to_compc)();
	NhlErrorTypes	(*compc_to_data)();
	NhlErrorTypes	(*win_to_compc)();
	NhlErrorTypes	(*compc_to_win)();
} TransObjLayerClassPart;

typedef struct _TransObjLayerClassRec {
	BaseLayerClassPart	base_class;
	TransObjLayerClassPart  trobj_class;
} TransObjLayerClassRec;

extern TransObjLayerClassRec transObjLayerClassRec;




#endif  /*_NTransObjP_h*/

