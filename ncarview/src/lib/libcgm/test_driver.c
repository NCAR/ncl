/*
 *	$Id: test_driver.c,v 1.6 2000-08-22 15:11:35 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
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
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

#include	<stdio.h>
#include	<fcntl.h>
#include	<ncarg/cgm_tools.h>
#include	<ncarg/cgmdef.h>

/*
 *		THE TEST DRIVER
 */

main(argc, argv)
	int	argc;
	char	*argv[];
{

	
	Cgm_fd	fd;

	Directory	*dir,
			*dir1;
	Instr	instr;
	int	i;

	dir = CGM_initMetaEdit(argv[1], 1440);
	if (!dir) {
		fprintf(stderr, "failed %d \n", i);
		exit(1);
	}
		


	CGM_printDirectory(dir);

}
		
