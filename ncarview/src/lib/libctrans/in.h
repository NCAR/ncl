/*
 *	$Id: in.h,v 1.6 2000-08-22 03:30:26 haley Exp $
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
#define	BUFSIZE 	1440	/*size of CGM buffer in bytes	*/
#define N 		-1	/*flag indicating any integer value	*/
#define STDIN		0
#define MAXID 		40	/*maximum number of CGM commands/class	*/

#define HEADERSIZE	4	/*size in bytes of NCAR header in record*/
#define STRINGSIZE	255	/*size in bytes of string as described in
				 *the ISO/DIS 8632, part 3, section pertaining
				 *to string data types			
				 */


#define	CONT_POSS	15	/* possistion of continuation flag	*/
#define CONT_BITS	1	/* number of bits in contiuation flag of 
				 * character string data type
				 */
#define C_PARM_BITS     15      /* number of bits in parameter count of
                                 * a long string
                                 */
#define C_PARM_POSS     14      /* possistion of bits of parm length for
                                 * a long string
                                 */
#define COMM_SIZE       2       /* size in bytes of a command header    */

/*abstract data type names for CGM element parameters */
enum ads { na,			/*none          */ 
           CI,			/*Colour Index  */
	   CD,			/*Colour direct */
	   E,			/*enumerated    */
	   I,			/*integer       */
	   IX,			/*index         */
	   P,			/*Point		*/
           R,			/*Real		*/
	   S,			/*String	*/
	   VDC,			/*VDC value	*/
	   D,			/*Data Record	*/
	   CO,			/*CI or CD 	*/
	   SPECIAL		/*multiple parameter types. Some commands
				 *have several types of parameters
				 *and require special processing
				 */
         };


