/*
 *      $Id: TransObj.h,v 1.5 1994-01-27 21:26:48 boote Exp $
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


#define NhlNtrOutOfRangeF	"trOutOfRangeF"
#define NhlCtrOutOfRangeF	"TrOutOfRangeF"

extern NhlLayerClass NhltransObjLayerClass;


#endif  /*_NTransObj_h*/
