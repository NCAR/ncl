/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_common_
#define	_common_

#define	BYTESIZE	8
#define	POWER16		65536.0		/*two to the power 16	*/
#define	POWER32		4294967300.0	/*two to the power 32	*/
#define	POWER15		32768.0
#define	POWER31		2147483650.0	/*two to the power 32	*/


	/*
	 *	Macro for extracting N bits from TARG stating at position
	 *	POSS counting from the left. E.g GETBITS(I, 4, 3) will
	 *	extract bits at bit position 4, 3, 2 right adjusted. 
	 *	This macro contains a conditional for number of bits greater
	 *	then 32 because some architechures such as Sun 4 with a sparc
	 *	cpu or pyramids cannot shift 32.
	 */

#define GETBITS(TARG,POSS,N) \
	((N)<32 ? (((TARG) >> ((POSS)+1-(N))) & ~(~0 << (N))) : \
	((TARG) >> ((POSS)+1-(N))) )

	/*
	 *	Inverse of the GETBITS macro. Place N bits from SRC into
	 *	TARG at position POSS. 
	 */
#define	PUTBITS(TARG, POSS, N, SRC) \
		(TARG) &= ~(~((~0) << (N)) << (((POSS)+1) - (N))); \
		(TARG) |= (((SRC) & ~((~0) << (N))) << (((POSS)+1) - (N))) 

#define MIN(X, Y)       ((X) < (Y) ? (X) : (Y))

#define ZERO_INDEX(X)   (X < 0 ? 0 : X)	

#endif common_
