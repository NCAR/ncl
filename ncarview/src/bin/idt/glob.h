/*
 *	$Id: glob.h,v 1.4 2008-07-27 03:22:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/


#ifndef	_glob_h_
#define	_glob_h_

extern	void	glob(
#ifdef	NeedFuncProto
	const char	*s,
	char	***r_argv,
	int	*r_argc
#endif
);

#endif	/* _glob_h_	*/
