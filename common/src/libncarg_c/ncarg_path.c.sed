/*
 *      $Id: ncarg_path.c.sed,v 1.2 2008-07-27 12:23:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*			     Copyright (C)  2000	                        		*
*	     University Corporation for Atmospheric Research		        *
*			     All Rights Reserved			                        *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	File:		ncarg_path.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 30 13:48:29 MDT 1991
 *
 *	Description:	The function GetNCARGPath returns a fully-qualified
 *			path name for a particulary directory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>
#include <errno.h>
#include <ncarg/c.h>
#include "ncarg_path.h"

#ifdef	cray
#include <fortran.h>
#endif

/*
 *	Create a string by concatenating the names $PREFIX and $postfix
 *	$postfix is converted to caps if necessary.
 */
static	char	*create_env_name(postfix)
	char	*postfix;
{
	char	*env;
	char	*s;

	env = malloc(strlen(PREFIX) + strlen(postfix) + 1);
	if (! env) {
		ESprintf(errno, "malloc(%s)",strlen(PREFIX)+ strlen(postfix)+1);
		return(NULL);
	}

	(void) strcpy(env, PREFIX);
	s = env + strlen(env);
	(void) strcat(env, postfix);

	while (*s) {
		if (islower(*s)) {
			*s = toupper(*s);
		}
		s++;
	}

	return(env);
}

/*
 * Function:	_NGResolvePath
 *
 * Description:	This function takes a pathname and returns a pathname with
 *		all "~"'s and environment "$var"'s resolved.
 *
 * In Args:	char 	*rawfname	fname as provided
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	char *
 * Side Effect:	
 */
const char
*_NGResolvePath
#if	NeedFuncProto
(
	const char	*rawfname	/* fname as provided	*/
)
#else
(rawfname)
	const char	*rawfname;	/* fname as provided	*/
#endif
{
	static char	fname[PATH_MAX];
	char		tmpfname[PATH_MAX];
	char		buffer[PATH_MAX];
	char		*piece = NULL;
	const char	*cs;
	struct passwd	*pw = NULL;
	int		first = 1;

	if(rawfname == NULL){
		return(NULL);
	}

	fname[0] = '\0';

	strcpy(tmpfname,rawfname);
	strcpy(buffer,_NGPATHDELIMITER);
	if(tmpfname[0] == buffer[0])
		strcpy(fname,_NGPATHDELIMITER);
	piece = strtok(tmpfname,_NGPATHDELIMITER);

	while(piece != NULL){

		if(first)
			first = 0;
		else
			strcat(fname,_NGPATHDELIMITER);

		switch(*piece){

			case '~':

				if(*(piece+1) != '\0')	/* different username */
					pw = getpwnam((piece+1));
				else			/* this username      */
					pw = getpwuid(getuid());

				if(pw == NULL){
					endpwent();
					ESprintf(E_UNKNOWN,
						"Unable to Resolve \'~\' in %s",
								rawfname);
					return(NULL);
				}
				strcat(fname,pw->pw_dir);
				endpwent();

				break;

			case '$':

				if(!strncmp(piece+1,PREFIX,strlen(PREFIX)))
					cs = _NGGetNCARGEnv(piece+ 1+
								strlen(PREFIX));
				else
					cs = getenv(piece + 1);

				if(cs == NULL){
					ESprintf(E_UNKNOWN,
						"Unable to Resolve %s in %s",
								piece,rawfname);
					return(NULL);
				}
				strcat(fname,cs);

				break;

			default:

				strcat(fname,piece);
		}

		piece = strtok(NULL,_NGPATHDELIMITER);
	}

	return(fname);
}

/*
 * Function:	GetNCARGEnv
 *
 * Description:		
 *			To get GetNCARGEnv to know about additional vars
 *			simply add them to the directvars,ngdirectdirs,
 *			ngrootdirs, or nglibdirs lists.
 *			These lists are NULL terminated.
 *
 *			The directvars is a list of env vars that ncarg uses
 *			but that don't necessarily fit in the ncarg root
 *			tree and the env name doesn't start with "NCARG_".
 *
 *			The ngdirectdirs are ncarg env vars that must be
 *			specified directly to override the default values.
 *			They can not be determined from other variables.
 *
 *			The ngrootdirs are ncarg env vars that specify
 *			filenames/dirnames that are exactly under $NCARG_ROOT if
 *			they are not specified directly.
 *
 *			The nglibdirs are ncarg env vars that specify
 *			filenames/dirnames that are exactly under
 *			$NCARG_ROOT/lib/ncarg if they are not specified
 *			directly.
 *
 *			The URL var is a special case, where instead of returning
 *			the path to a directory or a filename, it actually opens
 *			the file, reads the first line, and returns this (which 
 *			should be the URL for the NG documentation).
 *
 *			This function memorizes the return values that it
 *			reports so that if they are requested again it can
 *			just return the value instead of having to recalc.
 * In Args:	
 *	name
 *
 * Out Args:
 *
 * Return Values:
 *	NULL		=> Error, ESprintf() invoked.
 *	path		Else, the env var value is returned.
 *
 * Side Effects:
 */
const	char	*_NGGetNCARGEnv(name)
	const char	*name;
{
	static char	*env_vars[] = SED_ENV_DEFS;
	static char	*env_vals[(sizeof(env_vars)/sizeof(env_vars[0]))] =
					{ NULL };

	int		i=0;
	char		**current;

	static	char	buf[PATH_MAX];
	int		found = 0;
	int     slen;
	char		*env_name=NULL;
	char		*direct_val=NULL;
	char		localname[PATH_MAX];
	char		*s = localname;
	char		stmp[PATH_MAX];
	const char	*cs = NULL;
	FILE *fp;

	strcpy(localname,name);
	while (*s) {
		if (isupper(*s)) {
			*s = tolower(*s);
		}
		s++;
	}

	/*
	 * directvars section.
	 */
	current = env_vars;
	while(*current != NULL){
		if(!strcmp(*current,localname)){
			found = 1;
			break;
		}
		current+=5; i++;
	}
	if(!found){
		ESprintf(E_UNKNOWN, "Unknown environment specifier (%s)", name);
		return(NULL);
	}

	/*
	 * if it was previously calculated just return it...
	 */
	if(env_vals[i])
		return env_vals[i];

	/*
	 * get the actual Environment vars name...
	 */
	if(*(current+1))
		env_name = *(current+1);
	else
		env_name = create_env_name(localname);

	if(!env_name){
		ESprintf(E_UNKNOWN,"Unable to create Env Name?? for (%s)",name);
		return NULL;
	}

	/*
	 * See if the user set the env var directly...
	 */
	direct_val = getenv(env_name);
	if(env_name != *(current+1))
		free(env_name);

	/*
	 * if not, look for a default or calculate...
	 */
	if(!direct_val){
		/*
		 * Is there a parent directory specified?
		 */
		if(*(current+2)){
			cs = _NGGetNCARGEnv(*(current+2));
			if(!cs)
				return NULL;
			strcpy(buf,cs);
			strcat(buf,_NGPATHDELIMITER);
			strcat(buf,localname);
			direct_val = buf;
		}
		/*
		 * Is there a backup default specified?
		 */
		else if(*(current+3)){
			direct_val = *(current+3);

			/*
			 * See if there is a check file for this default.
			 */
			if(*(current+4)){
				struct stat	sbuf;

				strcpy(buf,direct_val);
				strcat(buf,*(current+4));

				if(stat(buf, &sbuf) < 0){
					ESprintf(E_UNKNOWN,
					"%s environment variable not set",
								env_name);
					return NULL;
				}
/*
#define	EMSGSTR "\n\nWarning: %s environment variable not set.\n\tAssuming %s as the value for\n\t%s.\n\tPlease see the 'ncargintro' man page for\n\tinformation on the %s environment variable.\n\n"
				fprintf(stderr,EMSGSTR,env_name,direct_val,
							env_name,env_name);
*/
			}
		}
		/*
		 * it's default value is NULL.
		 */
		else
			return NULL;
	} else {
		if(*(current+2)){
			 (void*)_NGGetNCARGEnv(*(current+2));
		}
	}

	cs = _NGResolvePath(direct_val);

	if(cs == NULL)
		return NULL;
/*
 * Check to see if we are prompting for URL path.  If so, then
 * later we will need to open a file to get the URL.
 */
	if( strcmp(localname,NCARGURL) ) {
		env_vals[i] = malloc(strlen(cs)+1);
		if(!env_vals[i]) {
			ESprintf(errno, "malloc(%s)",strlen(cs)+1);
			return NULL;
		}
		strcpy(env_vals[i],cs);
	}
/*
 * If the user wants the URL, then we need to open the file
 * to get it.
 */
	else {
		stmp[0] = '\0';
		fp = fopen(cs,"r");
		if( fp == (FILE *) NULL) {
			(void) ESprintf(errno, "fopen(%s)", cs);
			  return NULL;
		}
		(void)fgets(stmp,PATH_MAX-1,fp);
		if( !(slen = strlen(stmp)) ) {
			ESprintf(errno, "No URL found in file (%s)",cs);
			return NULL;
		}
		if( stmp[slen-1] == '\n' ) 
		  stmp[slen-1] = '\0';
		env_vals[i] = malloc(strlen(stmp)+1);
		if(!env_vals[i]) {
			ESprintf(errno, "malloc(%s)",strlen(stmp)+1);
			return NULL;
		}
		strcpy(env_vals[i],stmp);
	}
	return env_vals[i];
}

/*
 * Function:	GetNCARGPath
 *
 * Description:	This function just calls GetNCARGEnv.  The only reason I
 *		made a new name for it is that GetNCARGEnv returns all
 *		kinds of env var's, not just path names.
 *						jeff
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
const	char	*GetNCARGPath(path)
	const char	*path;
{
	return _NGGetNCARGEnv(path);
}

static	char	ErrMsgBuf[256];

/*
 *	return an error message when get_ncarg_path fails
 */
const	char	*get_ncarg_path_err()
{
	return(ErrMsgBuf);
}

/*
 * Function:	gngpat	- Get NCAR G Path
 *
 * Description:		This function provides a entry point for Fortran
 *			to the function _NGGetNCARGEnv(). The parameters
 *			pathlen and dirlen are provided by the fortran compiler.
 *			The fortran calling syntax should be:
 *
 *				call gngpat(path, dir, status)
 *
 * In Args:		
 *	*dir 		: The directory 
 *
 * Out Args:
 *	*path		: if status == 1 $path contains the path to $dir.
 *			  If status == -1 $path contains an error message.
 *	*status		: -1 on failure, else ok
 *
 * Return Values:
 *
 * Side Effects:
 */
void
NGCALLF(gngpat,GNGPAT)
#ifdef	cray
(path_, dir_, status)
	_fcd	path_;
	_fcd	dir_;
	int	*status;
#else
(path, dir, status, pathlen, dirlen)
	char	path[];
	char	dir[];
	int	*status;
	int	pathlen;
	int	dirlen;
#endif
{

#ifdef	cray
	int	pathlen = _fcdlen(path_);
	int	dirlen = _fcdlen(dir_);
	char	*path = path_;
	char	*dir = dir_;
#endif

	const char	*s;
	char	*dir_C = malloc(dirlen + 1);


	if (! dir_C) {
		*status = -1;
		strcpy(path, "Malloc failed");
		return;
	}

	strncpy(dir_C, dir, dirlen);
	dir_C[dirlen] = '\0';	/* strncpy does not null terminate	*/


	memset(path, 0, pathlen);
	if ((s = _NGGetNCARGEnv(dir_C)) == NULL) {
		*status = -1;
		strncpy(path, ErrMsgBuf, pathlen-1);
		return;
	}

	if ((int) strlen(s) >= pathlen) {
		*status = -1;
		strncpy(path, "Directory path too long", pathlen-1);
		return;
	}

	free((char *) dir_C);
	*status = 1;
	strcpy(path, s);
}
