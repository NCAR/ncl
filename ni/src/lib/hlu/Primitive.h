/*
 *      $Id: Primitive.h,v 1.1 2000-06-28 19:03:59 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Primitive.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 21 17:04:23 MDT 2000
 *
 *	Description:	Primitive public header file
 */
#ifndef _NPRIMITIVE_h
#define _NPRIMITIVE_h

#include <ncarg/hlu/Base.h>

typedef enum _NhlPolyType {
	NhlPOLYLINE = 0, NhlPOLYGON, NhlPOLYMARKER
} NhlPolyType;

#define NhlTPolyType		"PolyType"

#define NhlNprXArray		"prXArray"
#define NhlNprYArray		"prYArray"
#define NhlNprPolyType		"prPolyType"
#define NhlNprGraphicStyle	"prGraphicStyle"

#define NhlCprXArray		"PrXArray"
#define NhlCprYArray		"PrYArray"
#define NhlCprPolyType		"PrPolyType"
#define NhlCprGraphicStyle	"PrGraphicStyle"

extern NhlClass NhlprimitiveClass;

#endif  /* _NPRIMITIVE_h */
