/* 
 * $Id: filter.h,v 1.3 2000-08-22 03:30:23 haley Exp $
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
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#ifndef	cgm2ncgm_
#define	cgm2ncgm_

#define GETBITS(TARG,POSS,N) \
	((N)<32 ? (((TARG) >> ((POSS)+1-(N))) & ~(~0 << (N))) : \
	((TARG) >> ((POSS)+1-(N))) )



#define HEADERSIZE	4
#define NCAR_CGM_S	1440
#define	STDIN		0
#define STDOUT		1

/*
 *      positions of flag bits in *THIRD* byte of NCAR CGM header
 *      See section on NCAR CGM in "NCAR Graphics Installers Guide"
 */
#define	TYPE_POS	7	/* position of data type in header      */
#define	TYPE_LEN	4	/* number of bits               */

/*
 *      position of dataCount bits in first and second byte of NCAR
 *      CGM header, right adjusted
 */
#define COUNT_POS       15
#define COUNT_LEN       16


/*
 *      some NCAR CGM binary encoding defines
 */
#define LONGFORM        31      /* indicates long form of CGM command   */
#define PARM_BITS       5       /* number of bits in parameter list len */
#define PARM_POSS       4       /* possision of parmlen in instruction  */
#define ID_BITS         7       /* number of bits in element id         */
#define ID_POSS         11
#define CLASS_BITS      4       /* number of bits in element class      */
#define CLASS_POSS      15      /* possisition of class in instruction  */
#define PARM_BITS_L     15      /* number of bits in long parameter     */
#define PARM_POSS_L     14      /* possistion of parameter length in long*/
#define P_BITS          1       /* number of bits in partition flag     */
#define P_POSS          15      /* possistion of partition flag         */

#define CONT_POSS       15      /* possistion of continuation flag      */
#define CONT_BITS       1       /* number of bits in contiuation flag of
				 * character string data type
				 */

#define C_PARM_BITS     15      /* number of bits in parameter count of
				 * a long string
				 */
 
                
#endif

