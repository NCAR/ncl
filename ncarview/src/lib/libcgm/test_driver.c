/*
 *	$Id: test_driver.c,v 1.4 1992-09-01 23:40:58 clyne Exp $
 */
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
		
