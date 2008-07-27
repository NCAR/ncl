/*
 *	$Id: ncgm2cgm.c,v 1.13 2008-07-27 03:18:37 haley Exp $
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
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include 	<stdio.h>
#include 	<stdlib.h>
#include	<fcntl.h>
#include	<ncarg/cgm_tools.h>
#include	"filter.h"

/*	ncgm2cgm:	(NCAR CGM to vanilla CGM filter)
 *
 *
 *	Author	John Clyne 	(clyne@bierstadt.UCAR.EDU)
 *		7/8/88
 *
 *		This program removes the NCAR CGM 4 byte header from 
 *	NCAR CGM metafiles. The program acts as a filter taking input
 *	from stdin and writing to stdout. A command line option -s 
 * 	can be used to set the output record size. Otherwise the
 *	the default is used (1024)
 *
 * rev 1.01 clyne 10/2/89	: Incorporated cgm_tools library into program 
 *				for simplicity 	Moved under NCAR view hierarchy
 * rev 1.02 clyne 2/27/90       : blew up on valid (but non NCAR CGM ) records
 */


static	unsigned char	*outBuf;	/* the output buffer	*/
static	unsigned char	*outPtr;	/* pointer into outBuf	*/
static	unsigned	spaceAva;	/* bytes available in outBuf	*/
static	unsigned	blockSize = 1024;	/* output block size	*/

/*
 *	flush
 *	[internal]
 *
 *		Flush the output buffer
 */
static void	flush()
{
	int	to_write = blockSize - spaceAva;
	
	if (to_write) {	/* if buffer not empty	*/
		memset((char *) (outBuf+to_write), 0, (int) spaceAva);	

		if (fwrite((char *) outBuf, 1, blockSize, stdout) < 0) {
			perror("ncgm2cgm");
			exit(1);
		}
	}
}

static	void	usage()
{
	(void)fprintf(stderr,"Usage: ncgm2cgm [ -V ] [ -s <block_size> ]\n");
	exit(1);
}

main (argc,argv)
	int	argc;
	char	**argv;
{

	Cgm_fd	cgm_fd;
	unsigned char	*buf;		/* input buffer			*/
	unsigned	data_count;	/* num valid bytes in buf	*/
	unsigned	tmp;
	int	status;			/* read status			*/
	int	i;

	/*
	 * parse command line arguments
	 */
	for (i = 1; i < argc; i++) {
		char	*arg = argv[i];

		if (arg[0] == '-') {
			if (arg[1] == 'V') {
				PrintVersion(argv[0]);
				exit(0);
			}
			else if (arg[1] != 's') {/* look for '-s <size>'*/
				usage();
			}
			if (++i >= argc) {
				usage();
			}
			blockSize = atoi(argv[i]);
		}
		else
			usage();
	}

	spaceAva = blockSize;	/* record record size	*/

	/*
	 * open NCAR CGM file
	 */
	if ((cgm_fd = CGM_open("-", NCAR_CGM_S, "r")) < 0) {
		perror(argv[0]);
		exit(1);
	}

	/*
	 * malloc memory for read buffer and write buffer
	 */
	if ((buf = (unsigned char *) malloc (NCAR_CGM_S)) == NULL) {
		perror(argv[0]);
		exit(1);
	}
	if ((outBuf = (unsigned char *) malloc (blockSize)) == NULL) {
		perror(argv[0]);
		exit(1);
	}
	outPtr = outBuf;

	/*
	 * read until end of file
	 */
	while((status = CGM_read(cgm_fd, buf)) == NCAR_CGM_S) {

                /*
                 * make sure valid NCAR CGM. Only record type supported now
                 */
                switch (GETBITS(buf[2],TYPE_POS,TYPE_LEN)) {

                case    NCAR_CGM:
                        break;		/* valid CGM elements	*/

                case    HEADER:
                case    PRINTER:
                case    PRE_CGM:	/* skip over	*/
                        continue;
                        break;
                default:
			(void) fprintf(stderr, "%s: bogus metafile\n", argv[0]);
			exit(1);
                }

		/*
		 * extract the valid data count from the header
		 */
		tmp = buf[0] << 8 | buf[1];
		data_count = GETBITS(tmp, COUNT_POS, COUNT_LEN);

		if(data_count % 2) {	/* if odd then pad	*/
			data_count++;
		}

		
		/*
		 * write the record sans the header to a buffer
		 */
		if (fwrite(buf+HEADERSIZE, sizeof(char), data_count, stdout)<0){
			perror(argv[0]);
			exit(1);
		}
	}

	if (status != 0) {	/* if read did not fail because EOF	*/
		if (status < 0) {	/* system error			*/
			perror(argv[0]);
		}
		else {
			(void) fprintf(stderr, "%s: bogus metafile\n", argv[0]);
		}
	}

#ifdef	DEAD
	(void) flush();
#endif
	exit(0);
}

