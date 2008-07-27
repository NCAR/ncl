/*
 *      $Id: bitops.h,v 1.4 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
