/*
 *	$Id: bsd-sysv.h,v 1.3 1991-02-06 15:26:13 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifdef	SYSV
#if !defined (RS6000) && !defined(CRAY)
	extern	int	sprintf();
#endif
#else
	extern	char	*sprintf();
#endif
