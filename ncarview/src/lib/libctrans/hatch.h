/*
 *	$Id: hatch.h,v 1.1 1994-03-14 20:55:42 clyne Exp $
 */
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
