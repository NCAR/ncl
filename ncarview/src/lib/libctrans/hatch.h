/*
 *	$Id: hatch.h,v 1.4 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1994                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef	_hatch_h_
#define	_hatch_h_

#include "cgmc.h"
#include "commondev.h"

extern void	ComSimHatch(
#ifdef	NeedFuncProto
	Ptype		*p_list,
	long		n,
	IXtype		hatch_index,
	int		hatch_spacing,
	ComDev		*dev
#endif
);

#endif	/* _hatch_h_	*/
