/*
 *      $Id: nioError.c,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Error.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 2 14:01:23 MDT 1992
 *
 *	Description:	This file contains the error message functions
 *			used by the hlu's.  It has functionality simular
 *			to the ESprintf procedures, but an error object
 *			is formed to hide the implimentation from the
 *			user as well as to allow the user to configure
 *			the error system in the same way they configure
 *			plots.  (ie. by setting resources)
 *			It also incompases some added functionality not
 *			addressed by ESprintf - by allowing the user to send
 *			error messages to a specific file descriptor. And by
 *			default the error messages will be sent to stderr.
 *			Many of the functions in this file are based on
 *			the ESprintf functions written by John Clyne.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "nioVarArg.h"
#include "nioError.h"

#define	TABLELISTINC	10
#define	ERRLISTINC	32

static char		def_file[] = "stderr";
typedef void * NhlErrorLayer;
static NhlErrorLayer	errorLayer = NULL;


/************************************************************************
*									*
*	Public API for Error handling					*
*									*
************************************************************************/
/*
 * These var's are used to support the VarArg macro hack.
 */
static int HackUsed = False;
static struct {
	int	line;
	char	fname[_NhlMAXFNAMELEN];
} HackInfo;

/*
 * Function:	_NhlPErrorHack
 *
 * Description:	This function saves line and file information for NhlPerror
 *		to retrieve when it is called.  It is a gross ugly hack
 *		to allow a vararg macro call.
 *
 * In Args:	int	line;		line number
 *		char	*fname;		file name
 *
 * Out Args:	
 *
 * Scope:	Global Private -	It should only be called by the
 *					NHLPERROR macro.
 * Returns:	
 * Side Effect:	
 */
void
_NhlPErrorHack
#if	NhlNeedProto
(
	int		line,	/* line number	*/
	Const char	*fname	/* file name	*/
)
#else
(line,fname)
	int		line;	/* line number	*/
	Const char	*fname;	/* file name	*/
#endif
{
	HackUsed = True;

	HackInfo.line = line;
	strcpy(HackInfo.fname,fname);

	return;
}

/*
 * Function:	printerror
 *
 * Description:	This is the function used to report an error.
 *
 * In Args:	
 *		NhlErrorTypes	severity,	error severity
 *		int		errnum,		errornum in table
 *		char		*errstring,	fmt string
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	Const char * - the string it will print or buffer
 * Side Effect:	
 */
static Const char *printerror
#if     NhlNeedProto
(
	NhlErrorTypes	severity,	/* error severity	*/
	int		errnum,		/* errornum in table	*/
	NhlString	errstring	/* fmt string		*/
)
#else
(severity,errnum,errstring)
	NhlErrorTypes	severity;	/* error severity	*/
	int		errnum;		/* errornum in table	*/
	NhlString	errstring;	/* fmt string		*/
#endif
{
	NhlErrMsg	tmp;
	char		fname[_NhlMAXFNAMELEN];

	tmp.severity = severity;
	tmp.errorno = errnum;
	tmp.msg = errstring;

	tmp.sysmsg = NULL;

	if(HackUsed){
		tmp.line = HackInfo.line;
		tmp.fname = fname;
		strcpy(tmp.fname,HackInfo.fname);
		HackUsed = False;
	}
	else{
		tmp.line = 0;
		tmp.fname = NULL;
	}

	if (severity < NhlINFO)
		return NhlErrFPrintMsg(stderr,&tmp);
	return NULL;
}

/*
 * Function:	NhlPError
 *
 * Description:	This is the function used to report an error.  It takes
 *		a fmt string that works identical to printf.
 *
 * In Args:	
 *		NhlErrorTypes	severity,	error severity
 *		int		errnum,		errornum in table
 *		char		*fmt,		fmt string
 *		...				args for fmt string
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	Const char * - the string it will print or buffer
 * Side Effect:	
 */
Const char *NhlPError
#if     NhlNeedVarArgProto
(
	NhlErrorTypes	severity,	/* error severity	*/
	int		errnum,		/* errornum in table	*/
	char		*fmt,		/* fmt string		*/
	...				/* args for fmt string	*/
)
#else
(severity,errnum,fmt,va_alist)
	NhlErrorTypes	severity;	/* error severity	*/
	int		errnum;		/* errornum in table	*/
	char		*fmt;		/* fmt string		*/
	va_dcl				/* args for fmt string	*/
#endif
{
	va_list		ap;
	char		tbuf[NhlERRMAXMSGLEN];
	NhlString	errstr;

	if(fmt != NULL){

		VA_START(ap,fmt);
		(void)vsprintf(tbuf, fmt, ap);
		va_end(ap);

		errstr = tbuf;
	}
	else
		errstr = NULL;

	return printerror(severity,errnum,errstr);
}




/*
 * Function:	NhlErrSPrintMsg
 *
 * Description:	This function takes an error msg and formats it for printing
 *		It places the resulting string in the buffer provided.
 *
 * In Args:	
 *		Const NhlErrMsg	*msg		message to print
 *
 * Out Args:	
 *		char		*buffer,	buffer to print message to
 *
 * Scope:	Global Public
 * Returns:	Const char *
 * Side Effect:	
 */
char *
NhlErrSPrintMsg
#if	NhlNeedProto
(
	char		*buffer,	/* buffer to print message to	*/
	Const NhlErrMsg	*msg		/* message to print		*/
)
#else
(buffer,msg)
	char		*buffer;	/* buffer to print message to	*/
	Const NhlErrMsg	*msg;		/* message to print		*/
#endif
{
	char tbuf[NhlERRMAXMSGLEN];
	int	space = NhlERRMAXMSGLEN-1;
	int	tmp;

	if(msg->severity == NhlNOERROR)
		strcpy(buffer,"noerror");
	else if(msg->severity == NhlINFO)
		strcpy(buffer,"info");
	else if(msg->severity == NhlWARNING)
		strcpy(buffer,"warning");
	else
		strcpy(buffer,"fatal");
	space -= strlen(buffer);
	
	if((msg->line > 0) && (msg->fname != NULL)){
		sprintf(tbuf,":[\"%s\":%d]",msg->fname,msg->line);
		tmp = strlen(tbuf);
		strncat(buffer,tbuf,space);
		space -= tmp;
	}

	if(space <= 0) return buffer;

	if(msg->msg != NULL){
		tmp = strlen(msg->msg);
		strncat(buffer,":",space--);
		if(space <= 0) return buffer;
		strncat(buffer,msg->msg,space);
		space -= tmp;
	}

	if(space <= 0) return buffer;

	if(msg->errorno != NhlEUNKNOWN){
		sprintf(tbuf,":[errno=%d]",msg->errorno);
		tmp = strlen(tbuf);
		strncat(buffer,tbuf,space);
		space -= tmp;
	}

	if(space <= 0) return buffer;

	if(msg->sysmsg != NULL){
		strncat(buffer,":",space--);
		if(space <= 0) return buffer;
		strncat(buffer,msg->sysmsg,space);
	}

	return buffer;
}

/*
 * Function:	NhlErrFPrintMsg
 *
 * Description:	This function takes the given error msg and formats it for
 *		printing using NhlErrSPrintMsg.  And then prints it to
 *		the FILE* provided.
 *
 * In Args:	
 *		FILE		*fp;	file to print to
 *		Const NhlErrMsg	*msg	message to print
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	Const char *
 * Side Effect:	
 */
Const char *
NhlErrFPrintMsg
#if	NhlNeedProto
(
	FILE		*fp,	/* file to print to	*/
	Const NhlErrMsg	*msg	/* message to print	*/
)
#else
(fp,msg)
	FILE		*fp;	/* file to print to	*/
	Const NhlErrMsg	*msg;	/* message to print	*/
#endif
{
	static char tbuf[NhlERRMAXMSGLEN];

	if(fprintf(fp,"%s\n\r",NhlErrSPrintMsg(tbuf,msg)) < 0)
		fprintf(stderr,"Unable to print Error Messages???");
	fflush(fp);

	return tbuf;
}
