/*
 *	$Id: ncgm2cgm.c,v 1.7 1992-09-01 23:38:21 clyne Exp $
 */
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
 *	the default is used (1440)
 *
 * rev 1.01 clyne 10/2/89	: Incorporated cgm_tools library into program 
 *				for simplicity 	Moved under NCAR view hierarchy
 * rev 1.02 clyne 2/27/90       : blew up on valid (but non NCAR CGM ) records
 */


static	unsigned char	*outBuf;	/* the output buffer	*/
static	unsigned char	*outPtr;	/* pointer into outBuf	*/
static	unsigned	spaceAva;	/* bytes available in outBuf	*/
static	unsigned	recordSize;	/* NCAR record size in bytes	*/

main (argc,argv)
	int	argc;
	char	**argv;
{

	Cgm_fd	cgm_fd;
	unsigned char	*buf;		/* input buffer			*/
	unsigned	data_count;	/* num valid bytes in buf	*/
	unsigned	record_size = NCAR_CGM_S;	/* buffer size	*/
	unsigned	tmp;
	int	status;			/* read status			*/
	int	i;

	void		put();
	void		flush();

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
			record_size = atoi(argv[i]);
		}
		else
			usage();
	}

	spaceAva = recordSize = record_size;	/* record record size	*/

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
	if ((outBuf = (unsigned char *) malloc (record_size)) == NULL) {
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
#ifdef	DEAD
		(void) put(buf+HEADERSIZE, data_count);
#else
		if (fwrite(buf+HEADERSIZE, sizeof(char), data_count, stdout)<0){
			perror(argv[0]);
			exit(1);
		}
#endif
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

#ifdef	DEAD
/*
 *	put
 *	[internal]
 *
 *		buffer bytes until the output buffer is full. Then write
 *	the buffer to stdout
 * on entry
 *	*buf		: incoming data
 *	count		: num bytes in buf
 */
static void	put(buf, count)
	unsigned char	*buf;
	unsigned	count;
{
	int	to_copy;
	
	while (count != 0) {	/* while not all bytes are buffered	*/

		/*
		 * find out how much of data will fit into output buffer
		 */
		to_copy = (spaceAva > count ? count : spaceAva);

		bcopy((char *) buf, (char *) outPtr, to_copy);
		spaceAva -= to_copy;
		count -= to_copy;
		outPtr += to_copy;
		buf += to_copy;

		if (spaceAva == 0) {	/* if buffer is full 	*/
			if (write(STDOUT, (char *) outBuf, 
				(int) recordSize) != recordSize) {

				perror("ncgm2cgm");
				exit(1);
			}
			spaceAva = recordSize;
			outPtr = outBuf;
		}
	}
}


/*
 *	flush
 *	[internal]
 *
 *		Flush the output buffer
 */
static void	flush()
{
	int	to_write = recordSize - spaceAva;
	
	if (to_write) {	/* if buffer not empty	*/
		bzero((char *) (outBuf+to_write), (int) spaceAva);	

		if (write(STDOUT, 
			(char *) outBuf, (int) recordSize) != recordSize) {

			perror("ncgm2cgm");
			exit(1);
		}
	}
}
#endif

usage()
{
	(void)fprintf(stderr,"Usage: ncgm2cgm [-V | -s <output record size>]\n");
	exit(1);
}
