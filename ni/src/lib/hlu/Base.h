/*
 *      $Id: Base.h,v 1.5 1995-02-17 10:22:55 boote Exp $
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

extern int NhlGetParentWorkstation(
#if	NhlNeedProto
	int	pid
#endif
);

#endif  /* _NBase_h */
