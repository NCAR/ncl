
/*
 *      $Id: fill.h,v 1.4 2000-07-12 18:00:45 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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
	int 	cgmclass,
	int	id
#endif
);

#endif	/*	_fill_	*/
