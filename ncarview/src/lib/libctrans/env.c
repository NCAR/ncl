/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include	<stdio.h>

#ifdef SYSV
#include	<string.h>
#else
#include	<strings.h>
#endif SYSV

#include	"env.h"

/* 
 *	if the NCAR Graphics library path is not defined as a cpp 
 *	option use a default
 */
#ifndef	LIBPATH	
#define	LIBPATH	"/usr/local/lib"
#endif

/*	env.c
 *
 *	Authors:	John Clyne
 *			Don Middleton
 *
 *		
 *	This file contains procedures that return information
 *	about runtime environments, such as the type of graphics
 *	terminal you are using.
 *
 *	$NCARG
 *		If this environment variable is set, it points
 *		to the root of the installed libraries. Otherwise,
 *		the root of the installed libraries is assumed
 *		to be the path specified in the macro "LIBPATH".
 *
 *	$GRAPHCAP
 *		Names the graphic display device. This
 *		variable may be set to an absolute or
 *		relative pathname, if so desired. Otherwise,
 *		the name is searched for in $NCARG/graphcaps.
 *
 *	$FONTCAP
 *		Just like GRAHPCAP, but the default location
 *		is in $NCARG/fontcaps.
 *
 *
 *	11/88	Added support for install-time paths and
 *		absolute pathnames - Don Middleton 11/88
 *
 */ 

extern	char	*getenv();
extern	char	*calloc();


/*	getFcapname
 *
 *		get path to fontcap
 *	on entry:
 *		device : 	name of fontcap
 *	on exit
 *		return() = 	path to fontcap
 */
char *	getFcapname( device )
	char	*device;
{
	char	*get_libpath();
	char	*lib;
	char	*fcap;

	if ( device == (char *) NULL )
	{
		if ( (device = getenv("FONTCAP")) == (char *) NULL)
		{
			return(NULL);
		}
	}

	if ( *device == '/' || *device == '.' ) /* absolute path */
	{
		return( device );
	}
	else /* default fontcap libraries */
	{
		if ( (lib = get_libpath()) == NULL )
		{
			(void) fprintf(stderr,"Can't find NCAR libraries\n");
			exit(1);
		}


		fcap = (char *)calloc(	(unsigned)strlen(lib)+
				(unsigned)strlen(DEFAULT_FCAPDIR)+
				(unsigned)strlen(device) + 2,
				(unsigned)sizeof(char));
		fcap = strcat(fcap,lib);
		fcap = strcat(fcap,DEFAULT_FCAPDIR);
		fcap = strcat(fcap,"/");
		fcap = strcat(fcap,device);
		return(fcap);
	}
}

/*	getGcapname
 *
 *		get path to graphcap
 *	on entry:
 *		device : 	name of graphcap
 *	on exit
 *		return() = 	path to graphcap
 */
char *	getGcapname( device )
	char	*device;
{
	char	*get_libpath();
	char	*lib;
	char	*gcap;

	if ( device == (char *) NULL )
	{
		if ( (device = getenv("GRAPHCAP")) == (char *) NULL)
		{
			return(NULL);
		}
	}

	if ( *device == '/' || *device == '.' ) /* absolute path */
	{
		return( device );
	}
	else /* default graphcap libraries */
	{
		if ( (lib = get_libpath()) == NULL )
		{
			(void) fprintf(stderr,"Can't find NCAR libraries\n");
			exit(1);
		}

		gcap = (char *)calloc(	(unsigned)strlen(lib)+
				(unsigned)strlen(DEFAULT_GCAPDIR)+
				(unsigned)strlen(device) + 2,
				(unsigned)sizeof(char));
		gcap = strcat(gcap,lib);
		gcap = strcat(gcap,DEFAULT_GCAPDIR);
		gcap = strcat(gcap,"/");
		gcap = strcat(gcap,device);
		return(gcap);
	}
}

static	char *
get_libpath()
{
	char	*lib;
	
	if ( (lib = getenv(ENV_LIBPATH)) == NULL)
	{
		if ( !strcmp(LIBPATH, "SED_LIBRARY_PATH") )
		{
			return( NULL );
		}
		else
		{
			return(LIBPATH);
		}
	}
	else
	{
		return(lib);
	}
}
