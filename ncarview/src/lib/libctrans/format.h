/*
 *      $Id: format.h,v 1.4 2000-08-22 03:30:29 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1992                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
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
/*
 *	File:		format.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 10:26:54 MDT 1992
 *
 *	Description:	Prototype file for format.c
 */

#ifndef	_format_
#define	_format_

extern	int	formatinit(
#ifdef	NeedFuncProto
#endif
);

extern	int	itoa(
#ifdef	NeedFuncProto
	char *s,
	long n
#endif
);

extern	int	itooct(
#ifdef	NeedFuncProto
	char *s,
	long n
#endif
);

extern	int	itohex(
#ifdef	NeedFuncProto
	char *s,
	long n
#endif
);

extern	int	ftoa(
#ifdef	NeedFuncProto
	char	*s,
	float	f
#endif
);

extern	int	itotek(
#ifdef	NeedFuncProto
	char *s,
	long value
#endif
);


extern	int	formatcoord(
#ifdef	NeedFuncProto
	long	x,
	long	y,
	int	number
#endif
);

extern	int	formatveccnt(
#ifdef	NeedFuncProto
	long	count
#endif
);

extern	int	formatindex(
#ifdef	NeedFuncProto
	long	index,
	boolean	fillflag
#endif
);

extern	int	formatwidth(
#ifdef	NeedFuncProto
	int	width
#endif
);

extern	int	formatintensity(
#ifdef	NeedFuncProto
	long	data[3],
	int	count
#endif
);

#endif	/* _format_	*/
