/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

		
/*
 *	declarations to provide a boolean type
 */

#ifndef _boolean_
#define _boolean_

#ifndef	CRAY
typedef	unsigned char	boolean;
#else
typedef	unsigned int	boolean;
#endif

#ifndef	TRUE

#define FALSE	0
#define TRUE	!FALSE

#endif	TRUE
#endif
