/*
 *      $Id: TransObj.h,v 1.8 1996-06-19 16:56:22 dbrown Exp $
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
 *			The TransObjClass is responsible for managing 
 *			transformations DATA==>VIEWPORT definitions.
 */
#ifndef _NTransObj_h
#define  _NTransObj_h

#include <ncarg/hlu/Base.h>

#define NhlNtrOutOfRangeF	"trOutOfRangeF"
#define NhlCtrOutOfRangeF	"TrOutOfRangeF"
#define NhlNtrResolutionF	"trResolutionF"
#define NhlCtrResolutionF	"TrResolutionF"

extern NhlClass NhltransObjClass;


#endif  /*_NTransObj_h*/
