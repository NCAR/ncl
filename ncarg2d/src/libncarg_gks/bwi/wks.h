/*
 *	$Id: wks.h,v 1.4 2008-07-27 03:55:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
* 
*	NCAR Graphics - UNIX Version 3.1.2
*	Copyright (C) 1987, 1988, 1989, 1991, 2000
*	University Corporation for Atmospheric Research
*	All Rights Reserved
*     
***********************************************************************/

#define DEFAULT_GKS_OUTPUT	"gmeta"
#define DEFAULT_TRANSLATOR	"ctrans"

#define MAX_UNITS		256

/* Information below should, in general, not be altered */

#ifndef TRUE
#define TRUE			1
#endif

#ifndef FALSE
#define FALSE			0
#endif

#define RECORDSIZE		1440

#define MF_CLOSED		(FILE *) NULL

#define NO_OUTPUT		0
#define FILE_OUTPUT		1
#define PIPE_OUTPUT		2

#define MF_READ_ERROR		302
#define MF_WRITE_ERROR		303

/* For those systems that don't define the constants for lseek(). */

#ifndef L_SET
#define L_SET 0
#endif

#ifndef L_INCR
#define L_INCR 1
#endif

