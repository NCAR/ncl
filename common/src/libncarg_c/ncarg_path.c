
/*
 *      $Id: ncarg_path.c,v 1.2 1992-03-26 18:22:50 clyne Exp $
 */
/*
 *	File:		ncarg_path.c
 *
 *	Author:		Dave Kennison
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 30 13:48:29 MDT 1991
 *
 *	Description:	The function GetNCARGPath searches NCAR Graphics
 *			"parameter" file for the path name to a particular
 *			directory.
 */

#include <stdio.h>

/*LINTLIBRARY*/

/* The function "GetNCARGPath(paname)" returns a pointer to a string 
   representing
   the value assigned to the parameter named "paname" (if any) in the NCAR
   Graphics parameter file.  By default, the name of the parameter file is
   that embedded in the code below during the installation process, but that
   name may be overridden by putting the desired name in the environment
   variable NCARG_PARAMETER_FILE.  If a dollar sign is prepended to the name
   of the desired parameter, it prevents the value of NCARG_PARAMETER_FILE
   from being used, but is otherwise ignored; this plugs a security leak
   that arises in the use of "ncargsrc" (and also means that "ncargsrc" will
   not work properly when NCARG_PARAMETER_FILE is set - practically speaking,
   this is of no importance, since "ncargsrc" is for use on NCAR machines on
   which users will not set NCARG_PARAMETER_FILE). */

char *GetNCARGPath(paname)
char *paname;
{
  char *gtngto();
  char *ptflnm =  PARFIL_MACRO;
  char *ptemp;
  char *token;
  FILE *ptfile;
  char lstchr;
  int found=0;
  int dollar=0;

	extern	char	*getenv();

  if (*paname=='$') {
    dollar=1;
    paname++;
  }

  if (dollar==0 && (ptemp = getenv("NCARG_PARAMETER_FILE")) != NULL)
    ptflnm=ptemp;

  if (strcmp(paname,"PARFIL")==0)
    return (ptflnm);

  if ((ptfile=fopen(ptflnm,"r"))==NULL) {
    return((char *) NULL);
  }

  while (found==0) {
    token=gtngto(ptfile,&lstchr);
    if (lstchr==EOF) break;
    if (lstchr=='=' &&  strcmp(token,paname)==0) {
      found=1;
      token=gtngto(ptfile,&lstchr);
      if (lstchr!='\n' || *token == NULL) found=0;
    }
  }

  (void)fclose(ptfile);

  if (found==1)
    return (token);
  else
    return ((char*) NULL);
}


/* The function "gtngto" returns a pointer to a "token" retrieved from the
   input stream defined by "ptfile".  In this case, "token" refers to the
   concatenation of non-blank characters (up to 128 of them) from the input
   stream - up to, but not including the next equals sign, newline, or EOF.
   The character pointed to by "ptlchr" receives a copy of the equals sign,
   newline, or EOF that terminated the "token".  If a pound sign occurs on
   a line, it and all characters following it on the line are treated as
   blanks.  */

static char *gtngto(ptfile,ptlchr)
FILE *ptfile;
char *ptlchr;
{
  static char token[128];
  char flchar;
  int i=0;
  int psignf=0;

  for (;;) {
    flchar=getc(ptfile);
    switch (flchar) {
    case ' ': case '\t':
      break;
    case '#':
      psignf=1;
      break;
    case '=':
      if (psignf!=0) break;
    case '\n': case EOF:
      *ptlchr=flchar;
      token[i]='\0';
      return (token);
    default:
      if (psignf==0 && i<127) token[i++]=flchar;
      break;
    }
  }
}
