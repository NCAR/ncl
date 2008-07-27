/*
 *      $Id: rgb.c,v 1.2 2008-07-27 03:43:41 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	File:		rgb.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 12:07:13 MDT 1998
 *
 *	Description:	
 */
/* Copyright 1985, Massachusetts Institute of Technology */

/* reads from standard input lines of the form:
	red green blue name
   where red/green/blue are decimal values, and inserts them in a database.
 */
#include <stdio.h>
#ifdef cray
#include <sys/types.h>
#endif
#ifdef RS6000
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif
#include <ncarg/c.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>

char *ProgramName;

char *SysError ()
{
    return (errno >= 0 ? strerror(errno) : "?");
}

main(argc, argv)
    int argc;
    char **argv;
{
    char *dbname;
    char line[512];
    int red, green, blue;
    NGRGB rgb;
    NGdatum key, content;
    char name[512];
    int items;
    int lineno;
    int i, n;
    int fd;
    NGDBM *rgb_dbm;

    ProgramName = argv[0];

    if (argc == 2)
	dbname = argv[1];
    else{
	fprintf(stderr,"%s:Usage %s $outname < $rgbtext\n",
						ProgramName,ProgramName);
	exit(1);
    }

    strcpy (name, dbname);
    strcat (name, ".dir");
    fd = open (name, O_WRONLY|O_CREAT, 0666);
    if (fd < 0) {
	fprintf (stderr, 
		 "%s:  unable to create dbm file \"%s\" (error %d, %s)\n",
		 ProgramName, name, errno, SysError());
	exit (1);
    }
    (void) close (fd);

    strcpy (name, dbname);
    strcat (name, ".pag");
    fd = open (name, O_WRONLY|O_CREAT, 0666);
    if (fd < 0) {
	fprintf (stderr, 
		 "%s:  unable to create dbm file \"%s\" (error %d, %s)\n",
		 ProgramName, name, errno, SysError());
	exit (1);
    }
    (void) close (fd);

    rgb_dbm = NGdbm_open (dbname, O_RDWR, 0666);
    if (!rgb_dbm) {
	fprintf (stderr,
		 "%s:  unable to open dbm database \"%s\" (error %d, %s)\n",
		 ProgramName, dbname, errno, SysError());
	exit (1);
    }

    key.dptr = name;
    content.dptr = (char *) &rgb;
    content.dsize = sizeof (rgb);
    lineno = 0;
    while (fgets (line, sizeof (line), stdin)) {
	lineno++;
	items = sscanf (line, "%d %d %d %[^\n]\n", &red, &green, &blue, name);
	if (items != 4) {
	    fprintf (stderr, "syntax error on line %d\n", lineno);
	    fflush (stderr);
	    continue;
	}
	if (red < 0 || red > 0xff ||
	    green < 0 || green > 0xff ||
	    blue < 0 || blue > 0xff) {
	    fprintf (stderr, "value for %s out of range\n", name);
	    fflush (stderr);
	    continue;
	}
	n = strlen (name);
	for (i = 0; i < n; i++) {
	    if (isupper (name[i]))
		name[i] = tolower (name[i]);
	}
	key.dsize = n;
	rgb.red = (red * 65535) / 255;
	rgb.green = (green * 65535) / 255;
	rgb.blue = (blue * 65535) / 255;
	if (NGdbm_store (rgb_dbm, key, content, NGDBM_REPLACE)) {
	    fprintf (stderr, "%s:  store of entry \"%s\" failed\n",
		     ProgramName, name);
	    fflush (stderr);
	}
    }

    NGdbm_close(rgb_dbm);

    exit(0);
}
