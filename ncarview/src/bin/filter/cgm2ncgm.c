/*
 *	$Id: cgm2ncgm.c,v 1.13 2008-07-27 03:18:37 haley Exp $
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

/*	cgm2ncgm:	(vanilla CGM to NCAR CGM filter)
 *
 *
 *	Author	John Clyne 	(clyne@bierstadt.UCAR.EDU)
 *		7/8/88
 *
 *		This program installs the NCAR CGM 4 byte header into 
 *	"vanilla" CGM metafiles. The program acts as a filter taking input
 *	from stdin and writing to stdout. 
 *
 *	[10/4/89]
 *		Re-wrote to incorporate the cgm_tools library.
 */



static	unsigned char	*inBuf;	/* the output buffer	*/
static	unsigned char	*bufPtr;	/* pointer into outBuf	*/
static	unsigned	dataAva = 0;	/* bytes available in inBuf	*/
static	int		blockSize = 1024;	/* input block size	*/

/*
 *	get_data
 *	[internal]
 *		fill the input buffer
 * on entry
 *	count		: how many bytes are needed
 * on exit
 *	return		: -1 => error, 0 => EOF, else return count
 */
static	get_data(count)
{

	int	status;

	if (dataAva) {	/* move any existing data to begining of buffer	*/
		memmove((void *) inBuf, (const void *) bufPtr, (size_t) dataAva);
	}
	bufPtr = inBuf;

	/*
	 * read until we have enough data or EOF or error
	 */
	while (dataAva < count) {
		status = fread(inBuf+dataAva, 1, blockSize, stdin);
		if (status < 0) {
			return(status);
		}
		dataAva += status;
	}
	return(count);
}
/*
 *	get_next_instr
 *	[internal]
 *
 *		get the next cgm element from the vanilla CGM. 
 * on  exit
 *	*instr		: contains the CGM element
 *	return		: -1 => error, 0 => EOF, 1 => success
 */
static int	get_next_instr(instr)
	Instr	*instr;
{
	int	status;
	static	int	leftOver = 0;	/* num bytes NOT send last invocation */
	static	short	More = 0;	/* True if data is partitioned	*/
	unsigned	tmp;
	int	len;		/* bytes data needing to be extracted from buf*/

	/*
	 * initalize the Instr
	 */
	instr->more = 0;
	instr->data_length = 0;
	instr->data = instr->buf;	/* data will be in begining of buf */
	len = 0;

	/*
	 * if we had data left over from last invocation place it at 
	 * the head of the instr data buffer
	 */
	if (leftOver) {
		if (dataAva < leftOver) {
			if (get_data(leftOver) != leftOver) {
				return(-1);
			}
		}
		memmove((void *) instr->buf, (const void *) bufPtr, (size_t) leftOver);
		dataAva -= leftOver;	/* update buffers	*/
		bufPtr += leftOver;
		instr->data += leftOver;
	}

	/*
	 * if previous cgm element more flag was not set and there were
	 * not data left over from the previous invocation get the next
	 * command
	 */
	if (!More && !leftOver) {
		if (dataAva < 2) {	/* is enought data in buffer	*/
			if ((status = get_data(2)) != 2) {
				return(status); 	/* error or EOF	*/
			}
		}

		/* get the CGM command	*/
		tmp = bufPtr[0] << 8 | bufPtr[1];
		instr->cgmclass = GETBITS(tmp, CLASS_POSS, CLASS_BITS);
		instr->id = GETBITS(tmp, ID_POSS, ID_BITS);
		len = GETBITS(tmp, PARM_POSS, PARM_BITS); /* data length */
		dataAva -= 2;
		bufPtr += 2;
	}

	/*
	 * if the current cgm element contains a long form command or
	 * we are completing service for a cgm with multiple data partitions
	 * get the long form of the data length 
	 */
	if (len == LONGFORM || More)  {
		if (dataAva < 2) {
			if (get_data(2) != 2) {
				return(-1); 	/* error	*/
			}
		}

		tmp = bufPtr[0] << 8 | bufPtr[1];
		len = GETBITS(tmp, PARM_POSS_L, PARM_BITS_L);
		More = GETBITS(tmp, P_POSS, P_BITS);
		dataAva -= 2;
		bufPtr += 2;
	}

	instr->data_length = len + leftOver;

	if (instr->data_length > 32760) {	/* check for overflow	*/
		len = 32760 - leftOver;		/* data still in read buffer */
		leftOver = (instr->data_length) - 32760;
		instr->data_length = 32760;
	}
	else {
		leftOver = 0;
	}

	if (dataAva < len) {	
		if (get_data(len) != len)
			return(-1);	/* error	*/
	}	
	memmove((void *) instr->data, (const void *) bufPtr, (size_t) len);
	dataAva -= (len);
	bufPtr += (len);

	if (instr->data_length % 2) {
		dataAva -= 1;
		bufPtr += 1;
	}
	
	instr->more = (More || leftOver);
	instr->data = instr->buf;	/* reset data pointer to begining */

	return(1);
}

static	usage()
{
	(void)fprintf(stderr,"Usage: cgm2ncgm [ -V ] [ -s <block_size> ]\n");
	exit(1);
}

main (argc,argv)
	int	argc;
	char	**argv;
{

	Cgm_fd	cgm_fd;
	Instr	instr;			/* contains a cgm element	*/
	int	status;			/* read status			*/
	int	i;

	/*
	 * parse command line args
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

	/*
	 * open stdout for writing
	 */
	if ((cgm_fd = CGM_open("-", NCAR_CGM_S, "w")) < 0) {
		perror(argv[0]);
		exit(1);
	}

	/*
	 * malloc memory for write buffer and read buffer
	 */
	/* need enough mem for largest cgm element + 1 record	*/
	if ((inBuf = (unsigned char *) malloc (32760 + blockSize)) == NULL) {
		perror(argv[0]);
		exit(1);
	}


	/*
	 * while not EOF fetch cgm elements from the vanilla CGM and put
	 * them into a NCAR CGM
	 */
	while ((status = get_next_instr(&instr)) > 0) {

		if (instr.cgmclass > MAXCLASS || instr.id > MAXFUNCPERCLASS) {
			(void) fprintf(stderr, 
				"cgm2ncgm: warning: invalid CGM element - ");
			(void) fprintf(stderr,
				"class = %d, id = %d\n", instr.cgmclass, instr.id);
		}
		if (CGM_putInstr(cgm_fd, &instr) < 0) {
			(void)fprintf(stderr,"cgm2ncgm : error writing file\n");
			exit(1);
		}
		if (instr.cgmclass == DEL_ELEMENT && instr.id == END_MF_ID) {
			break;
		}

	}

	/*
	 * flush the output buffer
	 */
	CGM_flushOutputInstr(cgm_fd);

	if (status < 0) {
		(void) fprintf(stderr, "cgm2ncgm: warning: bogus metafile\n");
		exit(1);
	}
	exit(0);
}

