/*
 *	$Id: fort_c.h,v 1.2 1994-06-08 16:57:29 boote Exp $
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
 * This macro is used to resolve C/Fortran naming conventions on the
 * supported arch's.
 */
#ifndef	_GKCALLF
#ifdef	UNICOS
/* Brain dead cray loader */
#define _GKCALLF(reg,caps)	caps

#elif	defined(AIX) || defined(HPUX)
/* No munging of names - wow how unique */
#define _GKCALLF(reg,caps)     reg

#else   /* Regular old BSD */
#ifdef  __STDC__
#define _GKCALLF(reg,caps)     reg##_
#else
#define _GKCALLF(reg,caps)     reg/**/_
#endif  /* __STDC__ */
#endif  /* defined... */
#endif  /* _GKCALLF    */

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
