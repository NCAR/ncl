/*
 *      $Id: Base.h,v 1.10 1996-09-14 17:05:47 boote Exp $
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

#include <ncarg/hlu/hlu.h>

extern NhlClass NhlobjClass;
extern NhlClass NhlbaseClass;

#define NhlNULLOBJID 0

#define NhlTObjId		"ObjId"
#define NhlTObjIdGenArray	"ObjIdGenArray"


/* position enumeration */

#define NhlTPosition "Position"
typedef enum _NhlPosition {
	NhlTOP,
	NhlBOTTOM,
	NhlRIGHT,
	NhlLEFT,
	NhlCENTER
} NhlPosition;

/* justification enumeration */

#define NhlTJustification "Justification"
typedef enum _NhlJustification {
	NhlTOPLEFT,
	NhlCENTERLEFT,
	NhlBOTTOMLEFT,
	NhlTOPCENTER,
	NhlCENTERCENTER,
	NhlBOTTOMCENTER,
	NhlTOPRIGHT,
	NhlCENTERRIGHT,
	NhlBOTTOMRIGHT
} NhlJustification;

#endif  /* _NBase_h */
