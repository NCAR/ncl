/*
 *      $Id: format.h,v 1.2 1992-09-23 16:49:46 clyne Exp $
 */
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
