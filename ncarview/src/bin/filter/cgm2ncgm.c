/*
 *	$Id: cgm2ncgm.c,v 1.7 1992-09-01 23:38:19 clyne Exp $
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

/*	cgm2ncgm:	(vanilla CGM to NCAR CGM filter)
 *
 *
 *	Author	John Clyne 	(clyne@bierstadt.UCAR.EDU)
 *		7/8/88
 *
 *		This program installs the NCAR CGM 4 byte header into 
 *	"vanilla" CGM metafiles. The program acts as a filter taking input
 *	from stdin and writing to stdout. A command line option -s 
 * 	can be used to set the input record size. Otherwise the
 *	the default is used
 *
 *	[10/4/89]
 *		Re-wrote to incorporate the cgm_tools library.
 */



static	unsigned char	*inBuf;	/* the output buffer	*/
static	unsigned char	*bufPtr;	/* pointer into outBuf	*/
static	unsigned	dataAva = 0;	/* bytes available in inBuf	*/
static	unsigned	recordSize;	/* vanila CGM  record size in bytes */

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
		bcopy((char *) bufPtr, (char *) inBuf, (int) dataAva);
	}
	bufPtr = inBuf;

	/*
	 * read until we have enough data or EOF or error
	 */
	while (dataAva < count) {
		if ((status = read(STDIN, (char *) inBuf+dataAva, 
					(int) recordSize)) < 1) {

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
		bcopy((char *) bufPtr, (char *) instr->buf, (int) leftOver);
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
		instr->class = GETBITS(tmp, CLASS_POSS, CLASS_BITS);
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
	bcopy((char *) bufPtr, (char *) instr->data, (int) len);
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
	(void)fprintf(stderr,"Usage: cgm2ncgm [-V | -s <input record size>]\n");
	exit(1);
}

main (argc,argv)
	int	argc;
	char	**argv;
{

	Cgm_fd	cgm_fd;
	unsigned	record_size = NCAR_CGM_S;	/* input size	*/
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
			record_size = atoi(argv[i]);
		}
		else
			usage();
	}

	recordSize = record_size;

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
	if ((inBuf = (unsigned char *) malloc (32760 + record_size)) == NULL) {
		perror(argv[0]);
		exit(1);
	}


	/*
	 * while not EOF fetch cgm elements from the vanilla CGM and put
	 * them into a NCAR CGM
	 */
	while ((status = get_next_instr(&instr)) > 0) {

		if (instr.class > MAXCLASS || instr.id > MAXFUNCPERCLASS) {
			(void) fprintf(stderr, 
				"cgm2ncgm: warning: invalid CGM element - ");
			(void) fprintf(stderr,
				"class = %d, id = %d\n", instr.class, instr.id);
		}
		if (CGM_putInstr(cgm_fd, &instr) < 0) {
			(void)fprintf(stderr,"cgm2ncgm : error writing file\n");
			exit(1);
		}
		if (instr.class == DEL_ELEMENT && instr.id == END_MF_ID) {
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

