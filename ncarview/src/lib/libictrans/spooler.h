/************************************************************************
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
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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

#ifndef	_spooler_
#define	_spooler_

#define	MAX_FILTER	10	/* maximum filters in a filter chain	*/

#define	SPOOL_FILE	"ncarv_spool"	/* name of file containing spoolers */

typedef struct	{
	int	argc;
	char	**argv;
	} Args;

/*
 * a structure for storing information about a spooler chain
 */
typedef	struct	{
	char	*alias;		/* alias for the spooler (its name)	*/
	char	*input_string;	/* input string used to register spooler*/
	Args	*trans_args;	/* args to pass to the translator	*/
	Args	*filter;	/* list of filters			*/
	int	num_filt;	/* num filters				*/
	char	*file;		/* file to redirect filter output to	*/
	int	open_flags;	/* flags used when opening file		*/
	} Spooler;

/*
 * the spooler table
 */
typedef	struct	{
	Spooler	*spool;		/* the spooler list			*/
	int	size,		/* memory allocated to this table	*/
		num;		/* number of spoolers in table		*/
	} Spoolers;

typedef	int	Pipe[2];	/* pipe for filters to talk through	*/

#endif	/* _spooler_	*/
