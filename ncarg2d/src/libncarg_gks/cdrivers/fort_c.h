/*
 *	$Id: fort_c.h,v 1.1 1994-03-30 02:11:22 fred Exp $
 */
/*
 *      File:		fort_c.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Fri May  3 11:51:44 MDT 1991
 *
 *      Description:	Header file for Fortran/C interface.
 *		
 *	
 *
 */
#ifndef	_fort_c_
#define	_fort_c_

/*
 *	supported output device identifiers
 */
#define	DEV_CGM		1	/* not supported	*/
#define	DEV_X11P	7
#define	DEV_X11		8
#define	DEV_WISS	9	/* not supported	*/
#define	DEV_CTXT       10
#define	DEV_PS         20 	/* generic id for all PS drivers */
#define	DEV_PS_MIN     20       /* smallest id for the PS drivers */
#define	DEV_PS_MAX     31       /* largest id for the PS drivers */


#define	ERR_MSG_MAX	160	/* maximum error message size	*/

#endif	/*	_fort_c_	 */
