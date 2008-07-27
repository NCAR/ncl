/*
 *	$Id: options.h,v 1.7 2008-07-27 12:23:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1990                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*                          NCAR View V3.00alpha                         *
*                                                                       *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef	_options_
#define	_options_

/*
 *	maximum length of a string passed to AToArgv() by ParseEnvOptions()
 */
#define	MAX_ATOARGV_STRING	1024

typedef	unsigned long	BitMask;

/*
 * 	maximum number of option tables
 */
#define	MAX_TBLS	BITS(BitMask)

/*
 * 	initial malloc size for a small chunk of memory
 */
#define	SMALL_BLOCK	10

typedef	struct	{
	OptDescRec	*opt_desc_rec;	/* the option table	*/
	int		size; 		/* mem alloced to table */
	int		num;		/* num elements in table*/
	} OptTable;

#endif	/* _options_	*/
