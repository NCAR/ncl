/*
 *	$Id: in.c,v 1.17 2008-12-28 13:31:01 haley Exp $
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
#define	IN

#include	<stdio.h>
#include	<sys/types.h>
#ifdef	__FreeBSD__
#include	<sys/filio.h>
#else
#include	<sys/file.h>
#endif	/* __FreeBSD__ */
#include	<errno.h>
#include	<ncarg/cgm_tools.h>
#include	"in.h"
#include	"cgmc.h"
#include	"bitops.h"
#include	"fill.h"


/*	in.c:
 *		
 *		author : John Clyne
 *			 4/28/88
 *
 *		This file is responsible for reading in the CGM and 
 *	encoding its command elements and their data into the cgmc
 *
 */




/* following is a matrix indexed by CGM Command class and Id that
 * contains an indicator of that commands parameter type(s). The
 * definitions for the individual parameter types is specified 
 * in the ISO/DIS 8632 "Information Processing Systems Computer
 * Graphics" (here after to be referred to as IPSCG) part 3.
 */
struct key {
	enum ads p_type; 	/* parameter type (see cgmc.h for meaning)*/
       	int p_len;		/* number of args of type p_type
				 * (N implies unlimited amount)
				 */
	}Command[][MAXID] = {

	/* Class0*/
	{ 
	{na,	N},	/* no-op			*/	
	{S, 	1},	/* BEGIN METAFILE	*/
	{na, 	0},	/* END METAFILE		*/
	{S,	1},	/* BEGIN PIC 		*/
	{na,	0},	/* BEGIN PIC BODY	*/
	{na,	0},	/* END PICTURE		*/
	},

	/* Class1 */
        {
	{na,	0},	/* null id = 0		*/
	{I,	1},	/* METAFILE VERSION 	*/
	{S,	1},	/* METAFILE DESCRIPTION	*/
	{E,	1},	/* VDC TYPE		*/
	{I,	1},	/* INTEGER PRECISION	*/
	{I,	3},	/* REAL PRECISION	*/
	{I,	1},	/* INDEX PRECISION	*/
	{I,	1},	/* COLOUR PRECISION	*/
	{I,	1},	/* COLOUR INDEX PRECISION*/
	{CI,	1},	/* MAX COLOUR INDEX	*/
	{CD,	2},	/* COLOUR VALUE EXTENT	*/
	{SPECIAL,1},	/* METAFILE ELEMENT LIST*/
	{D,	N},	/* METAFILE DEFAULTS REP */
	{S,	N},	/* FONT LIST		*/
	{SPECIAL,1},	/* CHARACTER SET LIST	*/
	{E,	1},	/* CHAR CODING ANNOUNCER	*/
	},

	/* Class2 */ 
	{
	{na,	0},	/* null id = 0		*/
	{SPECIAL,1},	/* SCALING MODE		*/
	{E,	1},	/* COLOUR SELECTION MODE	*/		
	{E,	1},	/* LINE WIDTH SPEC MODE	*/
	{E,	1},	/* MARKER SIZE SPEC MODE */
	{E,	1},	/* EDGE WIDTH SPEC MODE	*/
	{P,	2},	/* VDC EXTENT		*/
	{CD,	1},	/* BACKROUND COLOUR	*/
	},
	
	/* Class3 */ 
	{
	{na,	0},	/* null id = 0		*/
	{I,	1},	/* VDC INTEGER PRECISION	*/
	{I,	3},	/* VDC REAL PRECISION	*/
	{CO,	1},	/* AUXILIARY COLOUR	*/
	{E,	1},	/* TRANSPARENCY		*/
	{P,	2},	/* CLIP RECTANGLE	*/
	{E,	1},	/* CLIP INDICATOR	*/
	},


	/* Class4 */
	{
	{na,	0},	/* null id = 0		*/
	{P,	N},	/* POLYINE		*/
	{P,	N},	/* DISJOINT POLYLINE	*/
	{P,	N},	/* POLYMARKER		*/
	{SPECIAL,1},	/* TEXT			*/
	{SPECIAL,1},	/* RESTRICTED TEXT	*/
	{SPECIAL,1},	/* APPEND TEXT		*/
	{P,	N},	/* POLYGON		*/
	{SPECIAL,1},	/* POLYGON SET		*/
	{SPECIAL,1},	/* CELL ARRAY		*/
	{SPECIAL,1},	/* GENERALIZED DRAW PRIM*/
	{P,	2},	/* RECTANGLE		*/
	{SPECIAL,1},	/* CIRCLE		*/
	{P,	3},	/* CIRCULAR ARC 3 POINT  */
	{SPECIAL,1},	/* CIRCULAR ARC 3 P CLOSE*/
	{SPECIAL,1},	/* CIRCULAR ARC CENTER	*/
	{SPECIAL,1},	/* CIRCULAR ARC C CLOSE	*/
	{P,	3},	/* ELLIPSE		*/
	{SPECIAL,1},	/* ELLIPTICAL ARC	*/
	{SPECIAL,1},	/* ELLIPTICAL ARC CLOSE	*/
	},

	/* Class5 */
	{
	{na,	0},	/* null id = 0		*/
	{IX,	1},	/* LINE BUNDLE INDEX	*/
	{IX,	1},	/* LINE TYPE		*/
	{R,	1},	/* LINE WIDTH		*/
	{CO,	1},	/* LINE COLOUR		*/
	{IX,	1},	/* MARKER BUNDLE INDEX	*/
	{IX,	1},	/* MARKER TYPE		*/
	{R,	1},	/* MARKER SIZE		*/
	{CO,	1},	/* MARKER COLOUR	*/
	{IX,	1},	/* TEXT BUNDLE INDEX	*/
	{IX,	1},	/* TEXT FONT INDEX	*/
	{E,	1},	/* TEXT PRECISION	*/
	{R,	1},	/* CHAR EXPANSION FACTOR*/
	{R,	1},	/* CHARACTER SPACING	*/
	{CO,	1},	/* TEXT COLOUR		*/
	{VDC,	1},	/* CHARACTER HEIGHT	*/
	{VDC,	4},	/* CHARACTER ORIENTATION*/
	{E,	1},	/* TEXT PATH		*/
	{SPECIAL,1},	/* TEXT ALIGNMENT	*/
	{IX,	1},	/* CHARACTER SET INDEX	*/
	{IX,	1},	/* ALTERNATE CHAR SET	*/
	{IX,	1},	/* FILL BUNDLE INDEX	*/
	{E,	1},	/* INTERIOR STYLE	*/
	{CO,	1},	/* FILL COLOUR		*/
	{IX,	1},	/* HATCH INDEX		*/
	{IX,	1},	/* PATTERN INDEX	*/
	{IX,	1},	/* EDGE BUNDLE INDEX	*/
	{IX,	1},	/* EDGE TYPE		*/
	{R,	1},	/* EDGE WIDTH		*/
	{CO,	1},	/* EDGE COLOUR		*/
	{E,	1},	/* EDGE VISIBILITY	*/
	{P,	1},	/* FILL REFERENCE POINT	*/
	{SPECIAL,1},	/* PATTERN TABLE	*/
	{VDC,	4},	/* PATTERN SIZE		*/	
	{SPECIAL,1},	/* COLOUR TABLE		*/
	{E,	N},	/* ASPECT SOURCE FLAGS	*/
	},

	/* Class6 */
	{
	{na, 	 0},	/* null id = 0		*/
	{SPECIAL,1},	/* ESCAPE		*/
	},

	/* Class7 */
	{
	{na,	 0},	/* null id = 0		*/
	{SPECIAL,1},	/* MESSAGE DATA		*/
	{SPECIAL,1},	/* APPLICATION DATA	*/
	}
};
	

static	Cgm_fd cgmFd;	/*  cgm file descriptor */
boolean Moreparm = FALSE;	/* true if CGM is partitioned	*/


/* 
 *	SetRecord:
 *		Advances to the specified record.
 *
 * on entry
 *	recnum		: record number to be read in
 * on exit
 *	return 		: status 
 */

int	SetRecord(recnum)
	int recnum;
{
	CGM_flushGetInstr(cgmFd);
	if (CGM_lseek(cgmFd, recnum, SEEK_SET) < 0) {
		ESprintf(errno,"CGM_lseek(%d, %d, %d)",cgmFd,recnum,SEEK_SET);
		return(-1);
	}
	return(0);
}

/* 
 *	Instr_Dec:
 *		extracts and decodes instruction and its parameters from 
 *		CGM_Buf. These are packaged and returned in the cgmc
 * on entry
 *	InitInput and SetRecord have been invoked
 * on exit
 *	cgmc 		: contains instruction and parameters
 *	return		: 1 => ok, 0 => eof, -1 => error
 */
int	Instr_Dec(cgmc) 
	CGMC *cgmc;
{
	enum ads p_type;	/* type of parameter(s) in CGM element	*/
	int p_len;		/* number of parameters of type *ads*	*/
	int retnum = 1;		/* return value				*/
	static Instr	instr;	/* the raw CGM instruction		*/
  
	/*
	 * fetch next raw CGM instruction from metafile
	 */
	if ((retnum = CGM_getInstr(cgmFd, &instr)) < 1) {
		if (retnum < 0 || (retnum == 0 && errno != 0)) {	
			ESprintf(errno,"Error fetching CGM element");
			/* else eof	*/
		}
		return(retnum);
	}

	/*
	 * assign the class and id of the CGM element to cgmc 
	 */
	cgmc->cgmclass = instr.cgmclass;
	cgmc->command = instr.id;
	cgmc->more = instr.more;

	p_type = Command[cgmc->cgmclass][cgmc->command].p_type;
	p_len = Command[cgmc->cgmclass][cgmc->command].p_len;

	/*
	 *	convert raw instr into cooked cgmc
	 */
	switch ((int) (p_type)) {
	case (int) CI  : 
		retnum = fill_CI(cgmc,&instr,p_len); 
		break;		   
	case (int) CD  : 
		retnum = fill_CD(cgmc,&instr,p_len); 
		break;
	case (int) E   : 
		retnum = fill_E(cgmc,&instr,p_len); 
		break;
	case (int) I   : 
		retnum = fill_I(cgmc,&instr,p_len); 
		break;
	case (int) IX  : 
		retnum = fill_IX(cgmc,&instr,p_len); 
		break;
	case (int) P   : 
		retnum = fill_P(cgmc,&instr,p_len); 
		break;
	case (int) R   : 
		retnum = fill_R(cgmc,&instr,p_len); 
		break;
	case (int) S   : 
		retnum = fill_S(cgmc,&instr); 
		break;
	case (int) VDC : 
		retnum = fill_VDC(cgmc,&instr,p_len); 
		break;
	/* currently parameters of type D are represented as string*/
	case (int) D   : 
		retnum = fill_D(cgmc,&instr,p_len); 
		break;
	case (int) CO  : 
		retnum = fill_CO(cgmc,&instr,p_len); 
		break;
	case (int) SPECIAL: 
		retnum = fill_special(cgmc,&instr,cgmc->cgmclass, cgmc->command);
		break;

	case (int) na  : 
		break;
	default  : 
		ESprintf(E_UNKNOWN,"Error decoding CGM element");
		return(-1);

	}	/* end switch*/

	if (retnum < 0) {
		ESprintf(
			E_UNKNOWN,"Error decoding CGM element [ %s ]",
			ErrGetMsg()
		);
		return(-1);
	}

	Moreparm = instr.more;	/* record more state	*/
	return (1);
}

/* 
 *	InitInput:
 *		initializes Input module. 
 *
 * on entry
 *	fd		: cgm file descriptor from CGM_open
 * on exit
 *	cgmFd  		: cgm_fd = cgmFd
 */

int	InitInput(fd)
	Cgm_fd	fd;
{
	cgmFd = fd;

	return (0);
}
