
/*
 *      $Id: fill.h,v 1.2 1992-11-19 21:49:24 clyne Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/

#ifndef	_fill_
#define	_fill_

#include "cgmc.h"


extern	int	fill_E(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_CD(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_CI(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_CO(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_D(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_I(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_IX(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_P(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_R(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_S(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr
#endif
);

extern	int	fill_VDC(
#ifdef	NeedFuncProto
	CGMC  	*cgmc,
	Instr	*instr,
	int	p_len
#endif
);

extern	int	fill_special(
#ifdef	NeedFuncProto
	CGMC 	*cgmc,
	Instr	*instr,
	int 	class,
	int	id
#endif
);

#endif	/*	_fill_	*/
