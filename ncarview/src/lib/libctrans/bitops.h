/*
 *      $Id: bitops.h,v 1.3 2000-08-22 03:30:29 haley Exp $
 */
/************************************************************************
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
 *	File:		bitops.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 10:43:30 MDT 1992
 *
 *	Description:	bitops.c prototype file.
 */
extern	long GetInt(
#ifdef	NeedFuncProto
	unsigned char *bufptr,
	int 	prec,
	boolean is_signed
#endif
);

extern	double GetReal(
#ifdef	NeedFuncProto
	unsigned char	*bufptr,
	boolean r_mode,
	int	expon_prec,
	int	man_prec
#endif
);


extern	int	Get_SI_int(
#ifdef	NeedFuncProto
	int 	prec,
	unsigned char *bufptr
#endif
);


extern	void	insert(
#ifdef	NeedFuncProto
	char	*dst,
	int	bit_start,
	int	bit_count,
	long	src,
	int	src_start_bit
#endif
);
