/*
 *      $Id: error.c,v 1.10 2008-07-27 12:23:45 haley Exp $
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
 *	File:		error.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Oct 15 20:46:23 MDT 1991
 *
 *	Description:	error.c is a generic error reporting module.
 *
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "c.h"

#define	TABLE_SIZE	10

/*
 *	 an error table. Error table[0] contains sys_errlist.
 */
typedef	struct	ErrTable_ {
	unsigned	start,		/* starting index for err_list	*/ 
			num;		/* num elements in err_list	*/
	const char	**err_list;	/* error messags		*/
	} ErrTable;

static	ErrTable	errTable[TABLE_SIZE];	/* all the error tables	*/
static	int		errTableNum = 0;	/* num error tables	*/
static	int		ErrorNumber;		/* current error num	*/
static	char		ErrorBuf[1024];		/* current error message*/
static	char		ErrorBufRet[1024];	/* error message returned*/
static	int		isInitialized = 0;	/* are we initialized?	*/

/*
 * this struct is necessary to implement the ESPRINTF() macro since
 * the C preprocessor does not handle variable argument macros
 */
static	struct	kludge_ {
	int	err_code;
	const char	*file;
	int	line;
	} kludge;
	
static	const char	*get_error(error)
	int	error;
{
	int	i;
	int	index;

	for (i=0; i<errTableNum; i++) {
		index = error - errTable[i].start;
		if (error >= errTable[i].start && index < errTable[i].num) {
			return(errTable[i].err_list[index]);
		}
	}

	return(NULL);
}


/*
 *	ESprintf()
 *
 *	ESprintf is exactly like sprintf/fprintf except that the formatted 
 *	text is written to an internal buffer instead of a user supplied 
 *	buffer/file.  'err_code' is an index into an error table. 0 - 1000
 *	are reserved for system errors. New error tables may be added 
 *	with ErrorList(). If an error message doesn't correspond to a 
 *	particular number the manifest constant 'E_UNKNOWN' should be
 *	passed.
 *
 * on entry
 *	err_code		: error number, E_UKNOWN if not known
 *	*format		: format string
 *	[, arg ...]	: var arg list
 * on exit
 *	return		: address of formated error message;
 */ 
#ifdef	__STDC__
const char	*ESprintf(unsigned err_code, const char *format, ...)
#else
const char	*ESprintf(err_code, format, va_alist)
	unsigned	err_code;
	const	char	*format;
	va_dcl
#endif
{
	va_list	ap;
	extern	int	errno;
	extern	char	*strerror(int);
	const char	*message;

	if (! isInitialized) {
		/*
		 * add the unix system/library error list
		 */
		(void) ErrorList(0, errno, (const char **) strerror(errno));
		isInitialized  = 1;
	}

	/*
 	 * record current error number
	 */
	ErrorNumber = err_code;


	/*
	 * deal with variable args
	 */
#ifdef	__STDC__
        va_start(ap, format);
#else
        va_start(ap);
#endif
        (void) vsprintf(ErrorBuf, format, ap);
        va_end(ap);

	/*
	 * see if its an error we know about. If so append the message.
	 */
	if ( (message = get_error(err_code)) ) {
		(void) strcat(ErrorBuf, " : ");
		(void) strcat(ErrorBuf, message);
	}

	return(ErrorBuf);
}


/*
 *	LFESprintf()
 *
 *	LFESprintf is identical except that it takes the additional
 *	arguments 'file' and 'line'.  file and line are expected to
 *	be the name of the source file and the line number where the 
 *	error occured. The string "FILE: <file>, LINE: <line>" is
 *	jammed into the head of the resultant error message 
 *
 * on entry
 *	err_code		: error number, E_UKNOWN if not known
 *	*file		: file name (or whatever you want)
 *	line		: line number where error occured.
 *	*format		: format string
 *	[, arg ...]	: var arg list
 * on exit
 *	return		: address of formated error message;
 */ 
#ifdef	__STDC__
const char	*LFESprintf(unsigned err_code, const char *file, 
					int line, const char *format, ...)
#else
const char	*LFESprintf(err_code, file, line, format, va_alist)
	unsigned	err_code;
	const	char		*file;
	int		line;
	const	char	*format;
	va_dcl
#endif
{
	va_list	ap;
	extern	int	errno;
	extern	char	*strerror(int);
	const char	*message;
	char		buf[1024];

	if (! isInitialized) {
		/*
		 * add the unix system/library error list
		 */
		(void) ErrorList(0, errno, (const char **) strerror(errno));
		isInitialized  = 1;
	}

	/*
 	 * record current error number
	 */
	ErrorNumber = err_code;


	/*
	 * deal with variable args
	 */
#ifdef	__STDC__
        va_start(ap, format);
#else
        va_start(ap);
#endif
        (void) vsprintf(buf, format, ap);
        va_end(ap);

	/*
	 * see if its an error we know about. If so append the message.
	 */
	if ( (message = get_error(err_code)) ) {
		(void) strcat(buf, " : ");
		(void) strcat(buf, message);
	}

	sprintf(ErrorBuf, "FILE: %s, LINE: %d, %s", file, line, buf);

	return(ErrorBuf);
}

/*
 *	ESprintfFirstPart()
 *
 *	This function is part of a kludge that allows the macro ESPRINTF() to 
 *	take a variable number of arguments.  It is not intended to 
 *	be called directly. In order to pass a variable argument list
 *	to a macro the variable part needs to be enclosed in a second
 *	set of parenthesis. eg
 *
 *		#define	VAR_MACRO (A,B,C) func(A,B,C)
 *
 *		VAR_MACRO(foo, bar, (var, arg, part))
 *
 *	which is expanded to 
 *
 *		func(foo, bar, (var,arg,part))
 *
 *	which is close but not quite right since (var,arg,part) will 
 *	evaluate var, arg, and part but only return part. But we can do
 *
 *		#define	VAR_MACRO (C) func C
 *
 *		VAR_MACRO((var, arg, part))
 *
 *	which expands to 
 *
 *		func(var, arg, part)
 *
 *	Which is correct but does not allow for mixing of fixed and variable
 *	length arg lists. Hence, ESprintfFirstPart() accepts the fixed arg
 *	list and ESprintfSecondPart() accepts the variable part. These
 *	functions are called in sequence by ESPRINTF()
 *
 *
 */
void	ESprintfFirstPart(err_code, file, line)
	int	err_code;
	const	char	*file;
	int	line;
{

	/*
	 * stash the relvant information for the subsequent call to 
	 * the variable arg function ESprintfSecondPart()
	 */
	kludge.err_code = err_code;
	kludge.file = file;
	kludge.line = line;
}

/*
 *	ESprintfFirstPart()
 *
 *	This function is the part of a kludge that allows the 
 *	macro ESPRINTF() to take variable arguments. 
 */
#ifdef	__STDC__
const char	*ESprintfSecondPart(const char *format, ...)
#else
char	*ESprintfSecondPart(format, va_alist)
	const char	*format;
	va_dcl
#endif
{
	va_list	ap;
	char	buf[1024];

	/*
	 * deal with variable args
	 */
#ifdef	__STDC__
        va_start(ap, format);
#else
        va_start(ap);
#endif
        (void) vsprintf(buf, format, ap);
        va_end(ap);

	return(LFESprintf(kludge.err_code, kludge.file, kludge.line, "%s",buf));
}
/*
 *	ErrGetMsg()
 *
 *	returns the current message
 */
const char	*ErrGetMsg()
{
	(void) strcpy(ErrorBufRet, ErrorBuf);
	return(ErrorBufRet);
}

/*
 *	ErrGetNum()
 *
 *	returns the current error number
 */
int	ErrGetNum()
{
	return(ErrorNumber);
}

/*
 *	ErrorList()
 *
 *	Adds an error list to the error table. 'start' should be the first
 *	valid error number for this table. The values 0 - 1000 are reserved.
 *	The index into 'err_list' is calculated by subtracting 'start'
 *	from the error number. Thus if you add a list with 'start' equal
 *	to 1001 and later invoke ESprintf with 'err_code' equal to 1001 the 
 *	error message referenced will be 'err_list[0]'
 *
 * on entry
 *	start		: first valid error number
 *	num		: number of elements in 'err_list'. 
 *	**err_list	: address of error list
 *
 * on exit
 *	return		: -1 => table full, else OK.
 */
int	ErrorList(start, num, err_list)
	unsigned	start,
			num;
	const	char		**err_list;
{

	if (errTableNum >= TABLE_SIZE -1) {
		return(-1);	/* table full	*/
	}

	errTable[errTableNum].start = start;
	errTable[errTableNum].num = num;
	errTable[errTableNum].err_list = err_list;
	errTableNum++;

	return(1);
	
}

