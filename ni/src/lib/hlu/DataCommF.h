/*
 *      $Id: DataCommF.h,v 1.1 1993-09-15 22:10:58 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataCommF.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug  6 15:26:40 MDT 1993
 *
 *	Description:	This file contains all the declarations that are
 *			needed for "friend" classes of the DataComm.
 */
#ifndef	_NDataCommF_H
#define	_NDataCommF_H

#include <ncarg/hlu/DataComm.h>

/*
 * Private API to be used by the DataMgr class only.
 */

extern NhlErrorTypes _NhlUpdateData(
#ifdef	NhlNeedProto
	int	dcommid,
	int	ditemid
#endif
);

extern void _NhlReleaseDMgr(
#ifdef	NhlNeedProto
	int	dspecid,
	int	ditemid
#endif
);

#endif	/* _NDataCommF_H */
