/*
 *	$Id: env.c,v 1.4 1992-04-03 20:57:09 clyne Exp $
 */
		

#include <ctype.h>
#include <stdio.h>

#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif SYSV

#include <ncarv.h>

/*	env.c
 *
 *	Authors:	John Clyne
 *			Don Middleton
 *                      Dave Kennison (modifications, July, 1991)
 *		
 *	This file contains procedures that return information
 *	about runtime environments, such as the type of graphics
 *	terminal you are using.
 *
 *	$GRAPHCAP
 *		Names the graphic display device. This
 *		variable may be set to an absolute or
 *		relative pathname, if so desired. Otherwise,
 *		the name is searched for in $NCARG/graphcaps.
 *
 *	$FONTCAP
 *              Just like GRAPHCAP, but the default location
 *		is in $NCARG/fontcaps.
 *
 *
 *	11/88	Added support for install-time paths and
 *		absolute pathnames - Don Middleton 11/88
 *
 *
 *      07/91   Code revised to use information from the "NCAR Graphics
 *              parameter file".  Threw out unused stuff, including the
 *              header file "env.h".
 */ 


extern	char	*getenv();
extern	char	*calloc();


/*      getFcapname
 *
 *		get path to fontcap
 *	on entry:
 *		device : 	name of fontcap
 *	on exit
 *		return() = 	path to fontcap
 */
char *  getFcapname( device )
	char	*device;
{
	char	*path;
	char	*fcap;

	if ( device == (char *) NULL )
	{
		if ( (device = getenv("FONTCAP")) == (char *) NULL)
		{
			return( (char *) NULL);
		}
	}

	if ( *device == '/' || *device == '.' ) /* absolute path */
	{
		return( device );
	}
	else /* default fontcap libraries */
	{
		if ( (path = GetNCARGPath("FONTCAPDIR")) == NULL )
		{
			/*
			 * can't find path so assume current directory
			 */
			path = ".";
		}


		fcap = (char *)calloc(	(unsigned)strlen(path)+
				(unsigned)strlen(device) + 2,
				(unsigned)sizeof(char));
		fcap = strcat(fcap,path);
		fcap = strcat(fcap,"/");
		fcap = strcat(fcap,device);
		return(fcap);
	}
}


/*      getGcapname
 *
 *		get path to graphcap
 *	on entry:
 *		device : 	name of graphcap
 *	on exit
 *		return() = 	path to graphcap
 */
char *  getGcapname( device )
	char	*device;
{
	char	*path;
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
		if ( (path = GetNCARGPath("GRAPHCAPDIR")) == NULL )
		{
			/*
			 * can't find path so assume current directory
			 */
			path = ".";
		}

		gcap = (char *)calloc(	(unsigned)strlen(path)+
				(unsigned)strlen(device) + 2,
				(unsigned)sizeof(char));
		gcap = strcat(gcap,path);
		gcap = strcat(gcap,"/");
		gcap = strcat(gcap,device);
		return(gcap);
	}
}

