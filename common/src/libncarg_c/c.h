/*
 *	$Id: c.h,v 1.5 1992-03-26 21:17:53 don Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_ncarv_
#define	_ncarv_

#ifdef	__STDC__
typedef	void *	Voidptr;
#else
#include <sys/types.h>
typedef	caddr_t	Voidptr;
#endif



/*
 *	structure for describing a valid option to buildOptionTable
 */
typedef	struct	_OptDescRec {
	char	*option;	/* name of option without preceeding '-' */
	int	arg_count;	/* num args expected by option		*/
	char	*value;		/* default value for the argument	*/
	char	*help;		/* help string for option		*/
	} OptDescRec;

/*
 *	structure for returning the value of an option
 */
typedef	struct	_Option {
	char		*option_name;	/* the options name		*/
	int		(*type_conv)();	/* option type converter	*/
	Voidptr		offset;		/* offset of return address	*/ 
	int		size;		/* size of option in bytes	*/
	} Option;

extern	int	NCARGCvtToInt();
extern	int	NCARGCvtToFloat();
extern	int	NCARGCvtToChar();
extern	int	NCARGCvtToBoolean();
extern	int	NCARGCvtToString();
extern	int	GetOption();
extern	int	LoadOptionTable();
extern	int	ParseOptionTable();
extern	void	PrintOptionHelp();

typedef	unsigned int	boolean;

#ifndef	TRUE
#define FALSE	0
#define TRUE	!FALSE
#endif	/* TRUE */


#define	BYTESIZE	8
#define	POWER16		65536.0		/*two to the power 16	*/
#define	POWER32		4294967300.0	/*two to the power 32	*/
#define	POWER15		32768.0
#define	POWER31		2147483650.0	/*two to the power 32	*/


	/*
	 *	Macro for extracting N bits from TARG stating at position
	 *	POSS counting from the left. E.g GETBITS(I, 4, 3) will
	 *	extract bits at bit position 4, 3, 2 right adjusted. 
	 *	This macro contains a conditional for number of bits greater
	 *	then 32 because some architechures such as Sun 4 with a sparc
	 *	cpu or pyramids cannot shift 32.
	 */

#define GETBITS(TARG,POSS,N) \
	((N)<32 ? (((TARG) >> ((POSS)+1-(N))) & ~(~0 << (N))) : \
	((TARG) >> ((POSS)+1-(N))) )

	/*
	 *	Inverse of the GETBITS macro. Place N bits from SRC into
	 *	TARG at position POSS. 
	 */
#define	PUTBITS(TARG, POSS, N, SRC) \
		(TARG) &= ~(~((~0) << (N)) << (((POSS)+1) - (N))); \
		(TARG) |= (((SRC) & ~((~0) << (N))) << (((POSS)+1) - (N))) 

#define ZERO_INDEX(X)   (X < 0 ? 0 : X)	




/*
 * error module defines
 */
#define	E_UNKNOWN	1000
extern	char	*ESprintf(/* int	errno, char *format, va_alist */);
extern	char	*ErrGetMsg();
extern	int	ErrGetNum();

/*
 * maintain backwords compatibility
 */
#define	ErrorGetMessage	ErrGetMsg
#define	ErrorGetNumber	ErrGetNum

extern	int	ErrorList(/* unsigned start, unsigned num, char **err_list */);


/*
 *	icmalloc defines
 */
#define	SMALL_MALLOC_BLOCK	10
extern	char	*icMalloc();
extern	char	*icRealloc();
extern	char	*GetNCARGPath();
extern	void	PrintVersion();

#endif
