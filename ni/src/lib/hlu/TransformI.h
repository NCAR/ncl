/*
 *      $Id: TransformI.h,v 1.1 1995-01-26 02:53:51 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransformI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 25 16:58:45 MST 1995
 *
 *	Description:	
 */
#ifndef	_NTransformI_h
#define	_NTransformI_h

#include <ncarg/hlu/Transform.h>

extern NhlBoolean _NhlIsOverlayMember(
#if	NhlNeedProto
	int	pid
#endif
);

#endif	/* _NTransformI_h */
