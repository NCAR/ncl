/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_options_
#define	_options_


/*
 * a list of valid  option styles
 */
typedef	enum	{
	OptIsArg, 	/* no argument, option is the arg. eg '-foo'	*/
	OptSepArg	/* argument follows specifier. eg '-foo doo'	*/
	} ArgType;

/*
 *	list of type converters. Options strings may be converted to 
 *	one of the following types
 */
typedef	enum	{
	StringType, IntType, FloatType, BoolType
	} OptType;

#define	IntType_	int
#define	StringType_	char *
#define	BoolType_	boolean
#define	FloatType_	float

/*
 *	structure for describing a valid option to buildOptionTable
 */
typedef	struct	_OptDescRec {
	char	*option;	/* name of option without preceeding '-' */
	ArgType	arg_type;	/* option style (OptIsArg, OptSepArg	*/
	char	*value;		/* default value for the argument	*/
	} OptDescRec;

/*
 *	structure for returning the value of an option
 */
typedef	struct	_Option {
	char		*option_name;	/* the options name		*/
	OptType		option_type;	/* its argument type		*/
	unsigned long	option_offset;	/* offset of return address	*/
	int		option_size;	/* size of option in bytes	*/
	} Option;

#endif
