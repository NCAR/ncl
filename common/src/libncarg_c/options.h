/*
 *	$Id: options.h,v 1.5 2000-07-11 21:58:07 haley Exp $
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
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this library; if not, write to the Free Software   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
