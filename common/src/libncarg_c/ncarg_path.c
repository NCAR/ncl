
/*
 *      $Id: ncarg_path.c,v 1.7 1992-09-29 17:49:17 ncargd Exp $
 */
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
#include <errno.h>
#include "c.h"
#include "ncarg_path.h"

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
 * Function:		dir_2_path
 *
 * Description:		This function attempts to services requests for
 *			generating a fully-qualified path name from a given
 *			directory name, $dir. The name resolution algorithm
 *			is as follows:
 *
 *			1) If the environment variable NCARG_$DIR, where
 *			$DIR is $dir in caps, is set return its contents.
 *			If not, proceed.
 *
 *			2) If the value of $dir is "tmp" return the value
 *			of the manifest constant DEFAULT_TMP. If not, proceed.
 *
 *			3) If the environment variable NCARG_ROOT is set
 *			return the string $NCARG_ROOT/$dir. If not,
 *			return NULL.
 *
 *			Examples:
 *
 *			If the env. var. 'NCARG_BIN' is set to '/usr/local/bin'
 *			dir_2_path("bin") will return '/usr/local/bin'.
 *
 *			If the env. var. 'NCARG_ROOT' is set to '/usr/local' 
 *			and the env. var. 'NCARG_LIB' is not set 
 *			dir_2_path("lib") will return '/usr/local/lib'.
 *
 *
 *			dir_2_path() invokes ESprintf() on error.
 *
 *			
 *
 * In Args:	
 *	dir		The directory name.
 *
 * Out Args:
 *
 * Return Values:
 *	NULL		=> error
 *	path		The directory path
 *
 * Side Effects:
 */
static	const char *dir_2_path(dir)
	const char *dir;
{
	static	char	buf[PATH_MAX];
	char	*env_name;
	char	*s;


	if (! (env_name = create_env_name(dir))) {
		return(NULL);
	}

	if ((s = getenv(env_name))) {
		free(env_name);
		return(s);
	}

	free(env_name);

	/*
	 * tmp is a special case
	 */
	if (strcmp("tmp", dir) == 0) {
		return(DEFAULT_TMP);
	}

	if ((s = getenv(ROOT_ENV))) {
		strcpy(buf, s);
		strcat(buf, "/");
		strcat(buf, dir);
		return(buf);
	}

	ESprintf(E_UNKNOWN, "%s environment variable not set", ROOT_ENV);
	return(NULL);
}

/*
 * Function:	GetNCARGPath
 *
 * Description:		Given a directory name return the complete path to
 *			this directory. GetNCARGPath() know about the following
 *			directories: $BINDIR, $LIBDIR, $TMPDIR, $DBDIR, 
 *			$FONTCAPDIR, $GRAPHCAPDIR, $EXAMPESDIR, $TESTSDIR, 
 *			$TUTORIALDIR, $XAPPDIR, and $INCDIR. For $BINDIR, $LIBDIR,
 *          $TMPDIR and $INCDIR
 *			GetNCARGPath() simply calls dir_2_path with dir as
 *			its argument. For the rest of the directories the
 *			path is created as:
 *
 *				dir_2_path($LIBDIR)/$NCARGDIR/$dir 
 *
 *			For example if LIBDIR="lib", NCARGDIR="ncarg",
 *			dir="xapp"  and the NCARG_LIB env variable is set
 *			to /usr/local/lib the returned value would be
 *			/usr/local/lib/ncarg/xapp.
 *
 * In Args:	
 *	dir		The directory.
 *
 * Out Args:
 *
 * Return Values:
 *	NULL		=> Error, ESprintf() invoked.
 *	path		Else, the directory path is returned
 *
 * Side Effects:
 */
const	char	*GetNCARGPath(dir)
	const char	*dir;
{
	/*
	 * these should all be passed in as macros from the makefile
	 */
	char	*bin = "bin";
	char	*lib = "lib";
	char	*tmp = "tmp";
	char	*inc = "include";
	char	*man = "man";
	char	*db = "database";
	char	*fontcap = "fontcaps";
	char	*graphcap = "graphcaps";
	char	*examples = "examples";
	char	*tests = "tests";
	char	*tutorial = "tutorial";
	char	*xapp = "xapp";
	char	*ncarg = "ncarg";

	const char	*libpath;
	static	char	buf[PATH_MAX];

	if ((strcmp(bin, dir) == 0) 
		|| (strcmp(lib, dir) == 0)
		|| (strcmp(tmp, dir) == 0)
		|| (strcmp(inc, dir) == 0)
		|| (strcmp(man, dir) == 0)) {

		return(dir_2_path(dir));
	}
	else if ((strcmp(db, dir) == 0)
		||  (strcmp(fontcap, dir) == 0)
		||  (strcmp(graphcap, dir) == 0)
		||  (strcmp(examples, dir) == 0)
		||  (strcmp(tests, dir) == 0)
		||  (strcmp(tutorial, dir) == 0)
		||  (strcmp(xapp, dir) == 0)) {

		libpath = dir_2_path(lib);
	}
	else {
		ESprintf(E_UNKNOWN, "Unknown directory specifier (%s)", dir);
		return(NULL);
	}

	if (! libpath) {
		return(NULL);
	}

	strcpy(buf, libpath);
	strcat(buf, "/");
	strcat(buf, ncarg);
	strcat(buf, "/");
	strcat(buf, dir);

	return(buf);
}
