/*
 *      $Id: Base.h,v 1.14 1997-01-17 18:57:17 boote Exp $
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

#define NhlNULLOBJID NhlDEFAULT_APP

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

#define	_NhlNnclData	"nclData"
#define	_NhlCnclData	"NclData"
#define	_NhlNguiData	"guiData"
#define	_NhlCguiData	"GuiData"
#define	_NhlNguiData2	"guiData2"
#define	_NhlCguiData2	"GuiData2"

#define NhlNobjAppObj	"objAppObj"
#define NhlCobjAppObj	"ObjAppObj"

#endif  /* _NBase_h */
