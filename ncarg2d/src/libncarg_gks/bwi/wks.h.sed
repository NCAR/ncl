/*
 *	$Id: wks.h.sed,v 1.4 1994-02-18 20:55:08 haley Exp $
 */
/***********************************************************************
* 
*	NCAR Graphics - UNIX Version 3.1.2
*	Copyright (C) 1987, 1988, 1989, 1991
*	University Corporation for Atmospheric Research
*	All Rights Reserved
*     
***********************************************************************/

/*
** This macro is one factor in the determination of the buffer size
** used for buffered I/O. The real value is edited into the file at
** system build time. Refer to the notes contained in wks.c.
*/

#define DEFAULT_GKS_BUFSIZE	SED_GKS_BUFFER_SIZE

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

