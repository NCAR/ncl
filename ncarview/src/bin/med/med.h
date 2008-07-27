/* 
 * $Id: med.h,v 1.7 2008-07-27 03:22:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

/*
 *	med.h
 *
 *	Author		John Clyne
 * 
 *	Date		Fri Jan 26 16:43:39 MST 1990
 *
 *	type declarations for med
 */
#ifndef	FALSE
#define	FALSE	0
#define	TRUE	~FALSE
#endif


#define	MAX_LINE_LEN	256 /* maximum length of a command line	*/

#define	PRI_USAGE	TRUE
#define	NO_PRI_USAGE	FALSE

#define	MED_FATAL	9
#define	MED_WARN	0

/* default prefix for split files names	*/
#define	SPLIT_PREFIX	"med"
#define	SPLIT_POSTFIX	".ncgm"

/*
 * Format of command table. (a command object)
 */
typedef	struct	{
	char	*c_name;	/* the name of the command	*/
	char    *c_help;        /* help string */
	char    *c_usage;	/* usage string */
	void     (*c_handler)(); /* function to call */
}	Cmd;

/*
 * a parsed command and its data
 */
typedef	struct	{
	char	*name;	/* the name given by user	*/
	int	add1,	/* first address		*/
		add2,
		add3;
	char	*file;	/* possible file argument	*/
	} CommandData;


/*
 * the global med data structure
 */
typedef	struct {
	FILE	*fp;		/* the command input file	*/
	CommandData	command_data;
	int	current_frame,
		last_frame;
	short	fromatty;
	char	*command_string;	/* commands if given on command line */
	Cmd	*c;			/* the command object		*/
	} MedData, MedDataPtr;

extern	Cmd	*getcmd (
#ifdef	NeedFuncProto
	char	*name
#endif
);
