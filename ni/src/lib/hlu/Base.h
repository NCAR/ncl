/*
 *      $Id: Base.h,v 1.7 1995-03-21 22:36:51 dbrown Exp $
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

extern NhlLayerClass NhlobjLayerClass;
extern NhlLayerClass NhlbaseLayerClass;

#define NhlNULLOBJID 0

#define NhlTObjId		"ObjId"
#define NhlTObjIdGenArray	"ObjIdGenArray"

extern int NhlGetParentWorkstation(
#if	NhlNeedProto
	int	pid
#endif
);

#endif  /* _NBase_h */
