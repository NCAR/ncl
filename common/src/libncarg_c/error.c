/*
 *      $Id: error.c,v 1.1 1992-03-26 18:24:25 clyne Exp $
 */
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
#include <varargs.h>
#include <stdio.h>
#include <errno.h>

#define	TABLE_SIZE	10

/*
 *	 an error table. Error table[0] contains sys_errlist.
 */
typedef	struct	ErrTable_ {
	unsigned	start,		/* starting index for err_list	*/ 
			num;		/* num elements in err_list	*/
	char		**err_list;	/* error messags		*/
	} ErrTable;

static	ErrTable	errTable[TABLE_SIZE];	/* all the error tables	*/
static	int		errTableNum = 0;	/* num error tables	*/
static	int		ErrorNumber;		/* current error num	*/
static	char		ErrorBuf[256];		/* current error message*/
static	int		isInitialized = 0;	/* are we initialized?	*/


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
char	*ESprintf(err_code, format, va_alist)
	unsigned	err_code;
	char	*format;
	va_dcl
{
	va_list	ap;
	extern	int	sys_nerr;
	extern	char	*sys_errlist[];
	char		*message;
	char		*get_error();

	if (! isInitialized) {
		/*
		 * add the unix system/library error list
		 */
		(void) ErrorList(0, sys_nerr, sys_errlist);
		isInitialized  = 1;
	}

	/*
 	 * record current error number
	 */
	ErrorNumber = err_code;


	/*
	 * deal with variable args
	 */
        va_start(ap);
        (void) vsprintf(ErrorBuf, format, ap);
        va_end(ap);

	/*
	 * see if its an error we know about. If so append the message.
	 */
	if (message = get_error(err_code)) {
		(void) strcat(ErrorBuf, " : ");
		(void) strcat(ErrorBuf, message);
	}

	return(ErrorBuf);
}

/*
 *	ErrGetMsg()
 *
 *	returns the current message
 */
char	*ErrGetMsg()
{
	return(ErrorBuf);
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
ErrorList(start, num, err_list)
	unsigned	start,
			num;
	char		**err_list;
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

static	char	*get_error(error)
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

