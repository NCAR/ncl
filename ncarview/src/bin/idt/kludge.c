/*
 *      $Id: kludge.c,v 1.1 1991-08-20 16:50:08 clyne Exp $
 */
/*
 *	File:		kludge
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Aug 20 16:28:33 MDT 1991
 *
 *	Description:	set up NCARG_PARAMETER_FILE environment variable so
 *			low level libraries can find path to ncarg parameter
 *			file.
 */
#include <stdio.h>

#define	PARM_FILE	"NCARG_PARAMETER_FILE"


#ifdef	TEST

extern	char	*getenv();
extern	char	*malloc();

main()
{
	char	*s;

	if (kludge() < 0) {
		fprintf(stderr, "kludge failed\n");
		exit (1);
	}

	if ((s = getenv (PARM_FILE)) == NULL) {
		fprintf(stderr, "kludge failed\n");
		exit (1);
	}

	printf("NCARG_PARMETER_FILE=%s\n", s);
	exit (0);
}

#endif	TEST

	


kludge()
{
	FILE	*fp;
	char	buf[80];
	char	*env_str;

	/*
	 * don't do anything if user already has NCARG_PARMETER_FILE
	 * env set.
	 */
	if (getenv(PARM_FILE)) {
		return(1);
	}

	if ((fp = popen("ncargpar PARFIL", "r")) == NULL) {
		return(-1);
	}

	if (fscanf(fp, "%s", buf) != 1) { 
		return(-1);
	}

	(void) pclose(fp);

	if ((env_str = malloc (
		strlen(PARM_FILE) + strlen("=") + strlen(buf) + 1)) == NULL) {

		return(-1);
	}

	(void) strcpy(env_str, PARM_FILE);
	(void) strcat(env_str, "=");
	(void) strcat(env_str, buf);

	return(putenv(env_str));
}
