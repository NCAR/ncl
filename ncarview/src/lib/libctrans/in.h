/*
 *	$Id: in.h,v 1.7 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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


