/*
 *	$Id: bsd-sysv.h,v 1.4 1992-04-03 20:41:07 clyne Exp $
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
