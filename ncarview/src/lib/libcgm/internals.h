/*
 *	$Id: internals.h,v 1.9 2008-07-27 03:22:38 haley Exp $
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
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_internals_
#define	_internals_

#include	<ncarg/c.h>

#define	DIR_2_ALLOC	100	/* initial directory size	*/
#define	NAME	"CGM_tools"	/* name of this library		*/
#define	rw_stdin	"-"	/* => read/write stdin/stdout	*/
#define	rw_pipe		"--"	/* => open a pipe		*/

#define	STDIN	0
#define	STDOUT	1

/*
 *	type of media (pipe, tty, file, memory_file)
 */
typedef	enum {
	Pipe, Tty, File, MemFile
	} Mtype;

/*
 *	This struct hold buffers and flags used by CGM_putInstr and
 *	CGM_getInstr. 
 */
typedef	struct	{
	unsigned char	*buf,		/* buffer for storing CGM records */	
			*buf_ptr;	/* pointer to buf		*/
	int		byte_count;	/* byte count for buf. If CGM_putInstr
					 * is used then this is the free count.
					 * If CGM_getInstr is used then this is
					 * the used count (index of buf_ptr)
					 */
	unsigned int	over_flow;	/* num bytes beyond MAX_CGM_INS_LEN
					 * being stored in users Instr
					 */
	boolean		more;		/* The CGM "more" partition flag*/	
	boolean		new_frame,
			beg_meta,
			end_meta;	/* NCAR record header flags	*/
	} Pg_struct;
/*
 *	this is the file descriptor table struct that records which files 
 *	are open.
 */
typedef	struct {
	unsigned	record_size;	/* record size of file		*/
	Mtype	mtype;			/* media type			*/
	FILE	*fp,
		*fpw;
	Cgm_fd	fd;			/* file descriptor for r/w	*/
	Cgm_fd	fdw;			/* write file descriptor if mtype
					 * is a Pipe
					 */
	Pg_struct	*pg_struct;	/* pointer to put/get instruction
					 * structs
					 */
	int	(*read)();
	int	(*write)();
	int	(*seek)();
	int	(*close)();
	int	(*flush)();
	} Cgm_tab;
/*
 *	some NCAR CGM binary encoding defines
 */
#define	BUFSIZE 	1440	/* size of CGM buffer in bytes		*/
#define PARM_BITS 	5	/* number of bits in parameter list len	*/	
#define PARM_POSS 	4	/* possision of parmlen in instruction	*/	
#define ID_BITS 	7	/* number of bits in element id		*/
#define ID_POSS 	11
#define CLASS_BITS 	4	/* number of bits in element class	*/
#define CLASS_POSS	15	/* possisition of class in instruction	*/
#define PARM_BITS_L 	15	/* number of bits in long parameter	*/
#define PARM_POSS_L	14	/* possistion of parameter length in long*/
#define P_BITS 		1	/* number of bits in partition flag	*/
#define P_POSS		15	/* possistion of partition flag		*/

#define	CONT_POSS	15	/* possistion of continuation flag	*/
#define CONT_BITS	1	/* number of bits in contiuation flag of 
				 * character string data type
				 */

#define C_PARM_BITS	15	/* number of bits in parameter count of
				 * a long string			
				 */
#define	C_PARM_POSS	14	/* possistion of bits of parm length for 
				 * a long string	
				 */

/*
 * more defines
 */
#define HEADERSIZE	4	/* size in bytes of NCAR header in record*/
#define COMM_SIZE	2	/* size in bytes of a command header	*/
#define STRINGSIZE	255	/* size in bytes of string as described in
				 * the ISO/DIS 8632, part 3, section pertaining
				 * to string data types			
				 */
#define LONGFORM	31	/* indicates long form of CGM command	*/
/*
 *	positions of flag bits in *THIRD* byte of NCAR CGM header	
 *	See section on NCAR CGM in "NCAR Graphics Installers Guide"
 */
#define	FRAME_POS	3 	/* new frame			*/
#define	BEG_MF_POS	2	/* new metafiles		*/
#define END_MF_POS	1	/* end metafile			*/
#define	LEN		1	/* number of bits in flag	*/

#define	TYPE_POS	7	/* position of data type in header	*/
#define	TYPE_LEN	4	/* number of bits		*/

/*
 *	position of dataCount bits in first and second byte of NCAR 
 *	CGM header, right adjusted
 */
#define	COUNT_POS	15
#define COUNT_LEN	16


#ifndef	MIN
#define	MIN(A,B)	((A) < (B) ? (A) : (B))
#endif	/* MIN	*/

#endif /* _internals	*/
