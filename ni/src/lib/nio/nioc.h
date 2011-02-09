/*
 *	$Id: nioc.h,v 1.3 2009-07-17 00:47:34 dbrown Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  2000                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*                          NCAR View V3.00alpha                         *
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


#ifndef	_ncarg_c_
#define	_ncarg_c_

#include <stdio.h>
#include <sys/types.h>
#include "nioNgSizeT.h"

/*
 * Fortran function macro.  This macro needs to surround any "C" reference
 * to a function that is written in fortran or is written in "C" to be
 * Fortran callable.
 */
#define APPEND_UNDERSCORE 1
#define NO_APPEND_UNDERSCORE 2
#define CAPS_NO_APPEND_UNDERSCORE 3

/* NGCALLF */

#ifdef FORTRAN_CALLING_METHOD

#if FORTRAN_CALLING_METHOD == APPEND_UNDERSCORE

#define NGCALLF(reg,caps)   reg##_

#elif FORTRAN_CALLING_METHOD == NO_APPEND_UNDERSCORE

#define NGCALLF(reg,caps)   reg

#elif FORTRAN_CALLING_METHOD == CAPS_NO_APPEND_UNDERSCORE

#define NGCALLF(reg,caps)   caps

#else

#define NGCALLF(reg,caps)   reg##_

#endif

#else

#define NGCALLF(reg,caps)   reg##_

#endif	/* NGCALLF */

/*
 * The Absoft ProFortran compiler munges common block names
 * differently than the way it handles Fortran subroutine names,
 * so here is where we deal with this.
 */
#ifndef NGCALLC
#if defined(AbsoftProFortran)
#define	NGCALLC(reg,caps)	_C##caps
#else
#define NGCALLC NGCALLF
#endif /* AbsoftProFortran else ... */
#endif /* NGCALLC */

typedef char    *NcargString;

#ifdef  UNICOS
#include <fortran.h>
#define NGstring            _fcd
#define NGCstrToFstr(cstr,len) ((cstr)?_cptofcd((char *)cstr,len):_cptofcd("",0))
#define NGFstrToCstr(fstr) (_fcdtocp(fstr))
#define NGFlgclToClgcl(flog)  (_ltob(&flog))
#define NGClgclToFlgcl(clog)  (_btol(clog))
#else
#define NGstring            char *
#define NGCstrToFstr(cstr,len) (char *)cstr
#define NGFstrToCstr(fstr) fstr
#define NGFlgclToClgcl(flog)  flog
#define NGClgclToFlgcl(clog)  clog
#endif

#define NGSTRLEN(cstr)      ((cstr)?strlen(cstr):0)

#ifdef	__STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif	/*	__STDC__	*/


#ifdef	__STDC__

typedef	void *	Voidptr;

#define	LABS(X)	labs(X)
#else

/*
 *	K&R C
 */
typedef	caddr_t	Voidptr;
#define	const		/* K&R C has no 'const'	*/

#define	LABS(X)	(long) (abs((int) (X)))

#endif	/*	__STDC__	*/

/*
 * C++ prototype protector
 */
#ifdef	__cplusplus
#define	NCARG_PROTO_BEGIN	extern "C" {
#define	NCARG_PROTO_END		}
#else
#define	NCARG_PROTO_BEGIN
#define	NCARG_PROTO_END	
#endif

typedef	unsigned int	boolean;

#ifndef	TRUE
#define FALSE	0
#define TRUE	!FALSE
#endif	/* TRUE */

NCARG_PROTO_BEGIN
/*
**
**	A R G U M E N T   V E C T O R S
**
*/

extern	char	**AToArgv(
#ifdef	NeedFuncProto
	const char      *str,
	const char      *prog_name,
	int     *argc
#endif
);

extern	void	FreeArgv(
#ifdef	NeedFuncProto
	char	**argv
#endif
);

/*
**
**	E R R O R   R E P O R T I N G
**
*/
#define	E_UNKNOWN	1000

#define	ESPRINTF(A,B)	ESprintfFirstPart(A, __FILE__, __LINE__), \
				ESprintfSecondPart B

/*
 * maintain backwords compatibility
 */
#define	ErrorGetMessage	ErrGetMsg
#define	ErrorGetNumber	ErrGetNum

/*ARGSUSED2*/
extern	const char	*ESprintf(
#ifdef	NeedFuncProto
	unsigned        err_code,
	const   char    *format,
	...
#endif
);

extern	const char	*LFESprintf(
#ifdef	NeedFuncProto
	unsigned	err_code,
	const char	*file,
	int		line,
	const char	*format,
	...
#endif
);

extern	void	ESprintfFirstPart(
#ifdef	NeedFuncProto
	int		err_code,
	const char	*file,
	int		line
#endif
);

extern const char    *ESprintfSecondPart(
#ifdef	NeedFuncProto
	const char	*format,
	...
#endif
);

extern	int	ErrorList(
#ifdef	NeedFuncProto
	unsigned start,
	unsigned num,
	const char **err_list
#endif
);

extern	const char	*ErrGetMsg(void);
extern	int	ErrGetNum(void);



/*
**
**	M I S C E L L A N Y
**
*/
extern	boolean	IsAsciiInt(
#ifdef	NeedFuncProto
	const char *s
#endif
);

extern	void USleep(
#ifdef	NeedFuncProto
	unsigned usec
#endif
);

/*
**
**	O P T I O N   P A R S I N G
**
*/

/*
 *	structure for describing a valid option to buildOptionTable
 */
typedef	struct	_OptDescRec {
	const char	*option;/* name of option without preceeding '-' */
	int	arg_count;	/* num args expected by option		*/
	char	*value;		/* default value for the argument	*/
	const char	*help;	/* help string for option		*/
	} OptDescRec;

/*
 *	structure for returning the value of an option
 */
typedef	struct	_Option {
	char		*option_name;	/* the options name		*/

			/* 
			 * option type converter	
			 */
	int		(*type_conv)(
#ifdef	NeedFuncProto
			const char *from, Voidptr to
#endif
			);

	Voidptr		offset;		/* offset of return address	*/ 
	int		size;		/* size of option in bytes	*/
	} Option;

typedef	struct	_EnvOpt {
	char	*option;		/* option name			*/
	char	*env_var;		/* coresponding enviroment var	*/
	} EnvOpt;

typedef	struct	Dimension2D_ {
	int	nx, ny;
	} Dimension2D;

extern	int	NCARGCvtToInt(
#ifdef	NeedFuncProto
	const char	*from,
	Voidptr		to
#endif
);

extern	int	NCARGCvtToFloat(
#ifdef	NeedFuncProto
	const char	*from,
	Voidptr		to
#endif
);

extern	int	NCARGCvtToChar(
#ifdef	NeedFuncProto
	const char	*from,
	Voidptr		to
#endif
);

extern	int	NCARGCvtToBoolean(
#ifdef	NeedFuncProto
	const char	*from,
	Voidptr		to
#endif
);

extern	int	NCARGCvtToString(
#ifdef	NeedFuncProto
	const char	*from,
	Voidptr		to
#endif
);

extern	int	NCARGCvtToDimension2D(
#ifdef	NeedFuncProto
	const char	*from,
	Voidptr		to
#endif
);

extern	int	OpenOptionTable(void);

extern	int	CloseOptionTable(
#ifdef	NeedFuncProto
	int	od
#endif
);

extern	int	GetOptions(
#ifdef	NeedFuncProto
	int	od,
	const Option	*options
#endif
);

extern	int	LoadOptionTable(
#ifdef	NeedFuncProto
	int	od,
	const OptDescRec	*optd
#endif
);

extern	void	RemoveOptions(
#ifdef	NeedFuncProto
	int	od,
	const OptDescRec	*optd
#endif
);

extern	int	ParseOptionTable(
#ifdef	NeedFuncProto
	int		od,
	int		*argc,
	char		**argv,
	const OptDescRec	*optds
#endif
);

extern	int	ParseEnvOptions(
#ifdef	NeedFuncProto
	int		od,
	const EnvOpt	*envv,
	const OptDescRec	*optds
#endif
);

extern	void	PrintOptionHelp(
#ifdef	NeedFuncProto
	int	od,
	FILE	*fp
#endif
);


/*
**
**	V E R S I O N
**
*/
extern	void	PrintVersion(
#ifdef	NeedFuncProto
	const char	*header
#endif
);

extern	const char *GetNCARGVersion(
#ifdef	NeedFuncProto
	void				 
#endif
);

extern	const char *GetNCLVersion(
#ifdef	NeedFuncProto
	void				 
#endif
);
/*
**
**	B I T   M A N I P U L A T I O N
**
*/


#ifndef	BITSPERBYTE
#define BITSPERBYTE     8
#endif

#ifndef	BITS
#define BITS(type)      (BITSPERBYTE * (int)sizeof(type))
#endif



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

#define ZERO_INDEX(X)   (X < 0 ? 0 : X)	


/*
**
**	N C A R  G   E N V I R O N M E N T
**
*/

extern	const char	*GetNCARGPath(
#ifdef	NeedFuncProto
	const char	*dir
#endif
);

extern	const char	*_NGGetNCARGEnv(
#ifdef	NeedFuncProto
	const char	*name
#endif
);

extern	const char	*_NGResolvePath(
#ifdef	NeedFuncProto
	const char	*rawfname
#endif
);

extern	char *getFcapname(
#ifdef	NeedFuncProto
	const char	*device
#endif
);

extern	char *getGcapname(
#ifdef	NeedFuncProto
	const char	*device
#endif
);

/*
**	R G B Database Support
**
*/

typedef struct _NGRGB{
	unsigned short red,green,blue;
} NGRGB;

/*
 * sdbm - ndbm work-alike hashed database library
 * based on Per-Aake Larson's Dynamic Hashing algorithms.
 * BIT 18 (1978).
 *
 * Copyright (c) 1991 by Ozan S. Yigit (oz@nexus.yorku.ca)
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * This file is provided AS IS with no warranties of any kind.  The author
 * shall have no liability with respect to the infringement of copyrights,
 * trade secrets or any patents by this file or any part thereof.  In no
 * event will the author be liable for any lost revenue or profits or
 * other special, indirect and consequential damages.
 */

#define NGDBLKSIZ 4096
#define NGPBLKSIZ 1024
#define NGPAIRMAX 1008			/* arbitrary on PBLKSIZ-N */
#define NGSPLTMAX	10			/* maximum allowed splits */
					/* for a single insertion */
#define NGDIRFEXT	".dir"
#define NGPAGFEXT	".pag"

typedef struct {
	int dirf;		       /* directory file descriptor */
	int pagf;		       /* page file descriptor */
	int flags;		       /* status/error flags, see below */
	long maxbno;		       /* size of dirfile in bits */
	long curbit;		       /* current bit number */
	long hmask;		       /* current hash mask */
	long blkptr;		       /* current block for nextkey */
	int keyptr;		       /* current key for nextkey */
	long blkno;		       /* current page to read/write */
	long pagbno;		       /* current page in pagbuf */
	char pagbuf[NGPBLKSIZ];	       /* page file block buffer */
	long dirbno;		       /* current block in dirbuf */
	char dirbuf[NGDBLKSIZ];	       /* directory file block buffer */
} NGDBM;

#define NGDBM_RDONLY	0x1	       /* data base open read-only */
#define NGDBM_IOERR	0x2	       /* data base I/O error */

/*
 * utility macros
 */
#define NGdbm_rdonly(db)		((db)->flags & NGDBM_RDONLY)
#define NGdbm_error(db)		((db)->flags & NGDBM_IOERR)

#define NGdbm_clearerr(db)	((db)->flags &= ~NGDBM_IOERR)  /* ouch */

#define NGdbm_dirfno(db)	((db)->dirf)
#define NGdbm_pagfno(db)	((db)->pagf)

typedef struct {
	char *dptr;
	int dsize;
} NGdatum;

extern NGdatum nullitem;

#ifdef __STDC__
#define proto(p) p
#else
#define proto(p) ()
#endif

/*
 * flags to dbm_store
 */
#define NGDBM_INSERT	0
#define NGDBM_REPLACE	1

/*
 * ndbm interface
 */
extern NGDBM *NGdbm_open proto((char *, int, int));
extern void NGdbm_close proto((NGDBM *));
extern NGdatum NGdbm_fetch proto((NGDBM *, NGdatum));
extern int NGdbm_delete proto((NGDBM *, NGdatum));
extern int NGdbm_store proto((NGDBM *, NGdatum, NGdatum, int));
extern NGdatum NGdbm_firstkey proto((NGDBM *));
extern NGdatum NGdbm_nextkey proto((NGDBM *));

/*
 * other
 */
extern NGDBM *NGdbm_prep proto((char *, char *, int, int));
extern long NGdbm_hash proto((char *, int));

#ifdef SVID
#include <unistd.h>
#endif

/*
 * important tuning parms (hah)
 */

#define NGBYTESIZ	(8)
#define NGSEEDUPS			/* always detect duplicates */
#define NGBADMESS			/* generate a message for worst case:
				   cannot make room after SPLTMAX splits */
/*
 * misc
 */
#define debug(x)

/*
**
**	M I S C E L L A N E O U S
**
*/

extern char	*NmuStrdup(
#ifdef NeedFuncProto
	const char	*str
#endif
);


NCARG_PROTO_END

#endif	/* _ncarg_c_	*/

