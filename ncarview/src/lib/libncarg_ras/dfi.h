/*
 *	$Id: dfi.h,v 1.5 1992-03-23 21:45:10 clyne Exp $
 */
/*****************************************************************************
* 
*			  NCSA HDF version 3.10r3
*				Dec 6, 1990
*
* NCSA HDF Version 3.10r3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
* 
* We ask, but do not require, that the following message be included in all
* derived works:
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.
* 
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
* 
*****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.5 $"
#endif
/*
 * Revision 1.4  1992/03/20  18:43:14  don
 * Added changes so that SGI ANSI C Compiler wouldn't barf.
 * Rewrote interface for resampling functions and added features.
 * Added a man page.
 *
 * Revision 1.3  92/02/21  12:49:30  clyne
 * ported to HP
 * 
 * Revision 1.2  91/08/16  11:11:26  clyne
 * *** empty log message ***
 * 
 * Revision 1.1  91/06/18  15:12:10  clyne
 * Initial revision
 * 
 * Revision 1.3  90/12/12  14:03:05  clyne
 * ported hdf to rs6000
 * 
 * Revision 1.2  90/12/11  18:28:19  clyne
 * added automatic detection for machine type.
 * 
 * Revision 1.1.1.1  90/12/11  14:24:02  clyne
 * hdf 3.10r3 distribution
 * 
 * Revision 1.1  90/12/11  14:24:00  clyne
 * Initial revision
 * 
 * Revision 3.14  90/10/02  14:16:49  clow
 * fixed error in the define of DFmovmem in the MIPSEL segment
 * 
 * Revision 3.13  90/09/18  15:09:10  clow
 * Added MIPSEL to 'list of approved machine types'
 * 
 * Revision 3.12  90/09/14  12:49:07  mfolk
 * *** empty log message ***
 * 
 * Revision 3.11  90/08/30  14:51:59  mfolk
 * Add DFMT_MIPSEL  0x4441 machine type and #ifdef section for MIPSEL.
 * This should cover DEC3100 needs.
 * 
 * Revision 3.10  90/07/31  13:30:15  mfolk
 * Added typedef double float64; to all machine types, so that this
 * dfi.h would work with fp2hdf as changed by INEL.  (Mike Folk)
 * 
 * Revision 3.9  90/07/16  11:25:56  clow
 * added #if's so that the compiler will complain if MACHINE is set wrongly.
 * 
 * Revision 3.8  90/07/05  17:00:08  clow
 * Changed the lower value of DF_TBUF 512
 * 
 * Revision 3.7  90/06/28  09:41:26  clow
 * Added segment for APOLLO machines
 * 
 * Revision 3.6  90/05/23  15:47:37  clow
 * added MACRO "DF_OPENERR" to give uniform error open checking,
 * especially for unbufferred I/O open
 * 
 * Revision 3.5  90/05/18  11:09:12  clow
 * added missing #endif
 * 
 * Revision 3.4  90/05/17  17:06:00  clow
 * added _fcd and _fcdtocp for LS FORTRAN so we don't have to go through
 * an extra layer of Fortran stubs
 * 
 * Revision 3.4  90/05/17  16:04:14  clow
 * added _fcd and _fcdtocp for LS FORTRAN so we don't have to go through
 * an extra layer of Fortran stubs
 * 
 * Revision 3.3  90/05/14  23:15:45  clow
 * added definition of MACRO "FCALLKEYW"
 * 
*/
/*-----------------------------------------------------------------------------
 * File:    dfi.h
 * Purpose: HDF internal header file
 * Invokes: stdio.h, sys/file.h
 * Contents: 
 *  Compilation parameters
 *  Machine-dependent definitions
 *  Flexibility definitions: i/o buffering, dynamic memory, structure i/o
 *  Size parameters
 * Remarks: To port to a new system, only dfi.h and Makefile need be modified.
 *          This file is included with user programs, but users do not see it.
 *---------------------------------------------------------------------------*/


#ifndef DF_MAGICK		/* avoid re-inclusion */

#define	DF_MAGICK	"\016\003\023\001" /* ^N^C^S^A */

#ifndef FILE
#include <stdio.h>
#endif /*FILE*/

/*--------------------------------------------------------------------------*/
/*          Compilation Parameters for Flexibility and Portability          */

/* modify this line to allow for machine dependencies */
/*#define	SUN*/

#undef	_KNOWN_
#if	defined(sun) && ! defined(_KNOWN_)
#ifndef	SUN
#define	SUN
#endif
#define	_KNOWN_
#endif

#if	defined(ultrix) && ! defined(_KNOWN_)
#ifndef	MIPSEL
#define	MIPSEL
#endif
#define	_KNOWN_
#endif

#if	defined(sgi) && defined(mips) && ! defined(_KNOWN_)
#ifndef	IRIS4
#define	IRIS4
#endif
#define	_KNOWN_
#endif

#if	defined(cray)
#ifndef	UNICOS
#define	UNICOS
#endif
#define	_KNOWN_
#endif

#ifndef	_KNOWN_
#ifndef	RS6000
#define	RS6000
#endif
#endif


/**IMPORTANT** this is now in the in the makefile */

#if !defined(SUN) && !defined(SUN386) && !defined(VMS) && !defined(ALLIANT)
#if !defined(IRIS4) && !defined(MAC) && !defined(UNICOS) && !defined(MIPSEL)
#if !defined(RS6000)
if there is error on this line, the MACHINE type is defined wrong!!!
#endif
#endif
#endif

/* modify this line for buffered/unbuffered i/o */
#define	DF_BUFFIO

/* modify this line for dynamic/static memory allocation */
#define	DF_DYNAMIC

/* modify this line if structures cannot be read/written as is */
#undef	DF_STRUCTOK		/* leave it this way - hdfsh expects it */

/* Current version number */
#define	DFVERSION   3.00

/*--------------------------------------------------------------------------*/
/*                              MT/NT constants                             */
/*      four MT nibbles represent int, float, double, uchar                 */
#define	DFMT_SUN        0x1111
#define	DFMT_ALLIANT    0x1111
#define	DFMT_IRIS4      0x1111
#define DFMT_APOLLO	0x1111
#define	DFMT_UNICOS     0x3331
#define	DFMT_CTSS       0x3331
#define	DFMT_VAX        0x2221
#define DFMT_MIPSEL     0x4441
#define	DFMT_PC         0x4144	/* note byte swapping ??? */
				/* check this... */
#define	DFMT_MAC        0x1111
#define DFMT_SUN386	0x1444

#define	DFNT_VERSION    1	/* current version of NT info */

/* type info codes */
#define	DFNT_UINT       1
#define	DFNT_INT        2
#define	DFNT_UCHAR      3
#define	DFNT_CHAR       4
#define	DFNT_FLOAT      5
#define	DFNT_DOUBLE     6

/* class info codes for int */
#define	DFNTI_MBO       1	/* Motorola byte order 2's compl */
#define	DFNTI_VBO       2	/* Vax byte order 2's compl */
#define	DFNTI_IBO       4	/* Intel byte order 2's compl */

/* class info codes for float */
#define	DFNTF_IEEE      1	/* IEEE format */
#define	DFNTF_VAX       2	/* Vax format */
#define	DFNTF_CRAY      3	/* Cray format */
#define	DFNTF_PC        4	/* PC floats - flipped IEEE */

/* class info codes for char */
#define	DFNTC_BYTE      0	/* bitwise/numeric field */
#define	DFNTC_ASCII     1	/* ASCII */
#define	DFNTC_EBCDIC    5	/* EBCDIC */

/* array order */
#define	DFO_FORTRAN     1	/* column major order */
#define	DFO_C           2	/* row major order */

/*--------------------------------------------------------------------------*/
/*                      Machine dependencies                                */
#ifdef PC
#ifndef O_RDONLY
#include <fcntl.h>
#define	L_INCR  1
#endif /*O_RDONLY*/
typedef	int int16;
typedef	unsigned int uint16;
typedef	long int int32;
typedef	double float32;
#define	DFmovmem(from, to, len) memcpy(to, from, len)
#undef DF_STRUCTOK                  /* structure writing will not work on PC */
#undef DF_BUFFIO                    /* unbuffered i/o is faster */
long longswap();                    /* provided elsewhere */

    /* in the next 3 lines, p is recast so right number of bytes passed */
    /* ### Note that all calls modify p.  Need to use temporary ptr */
#define	UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define	INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define	INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define	UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	DF_CREAT(name, prot) creat(name, prot)
#define	DF_MT   DFMT_PC
#endif /*PC*/


#ifdef UNICOS
#ifndef O_RDONLY
#include <sys/fcntl.h>              /* for unbuffered i/o stuff */
#define	L_INCR  1
#endif /*O_RDONLY*/
typedef	int int16;
typedef	int uint16;
typedef	int int32;
typedef	float float32;
typedef double float64;
#define	DFmovmem(from, to, len) memcpy(to, from, len)
#undef DF_STRUCTOK          /* cannot directly read/write structures */
#define	DF_CAPFNAMES            /* fortran names are in all caps */
#define	UINT16READ(p, x)    { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define	INT16READ(p, x)     { x = (*p++)<<8; x |= (*p++) & 255; }
#define	INT32READ(p, x)     { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
                                x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define	UINT16WRITE(p, x)   { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT16WRITE(p, x)    { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT32WRITE(p, x)    { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;   \
                                *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	DF_CREAT(name, prot) creat(name, prot)
#define	DF_MT   DFMT_UNICOS
#endif /*UNICOS*/


#ifdef SUN
#if ! defined mc68010 && ! defined mc68020 && ! defined mc68030
#undef DF_STRUCTOK
#endif
#include <sys/file.h>               /* for unbuffered i/o stuff */
typedef	short int16;
typedef	unsigned short uint16;
typedef	long int32;
typedef	float float32;
typedef double float64;
#define	DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define	UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define	INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define	INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define	UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define	DF_CREAT(name, prot) creat(name, prot)
#define	DF_MT   DFMT_SUN
#endif /*SUN*/

#ifdef SUN386
#undef DF_STRUCTOK
#include <sys/file.h>               /* for unbuffered i/o stuff */
typedef	short int16;
typedef	unsigned short uint16;
typedef	long int32;
typedef	float float32;
typedef double float64;
#define	DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define	UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define	INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define	INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define	UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define	DF_CREAT(name, prot) creat(name, prot)
#define	DF_MT   DFMT_SUN386
#endif /* SUN386 */

#ifdef ALLIANT
#include <sys/file.h>               /* for unbuffered i/o stuff */
typedef	short int16;
typedef	unsigned short uint16;
typedef	long int32;
typedef	float float32;
typedef double float64;
#define	DFmovmem(from, to, len) bcopy(from, to, len)
#ifndef DF_STRUCTOK
#define	UINT16READ(p, x)    { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define	INT16READ(p, x)     { x = (*p++)<<8; x |= (*p++) & 255; }
#define	INT32READ(p, x)     { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
                                x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define	UINT16WRITE(p, x)   { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT16WRITE(p, x)    { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define	INT32WRITE(p, x)    { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;   \
                                *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define	DF_CREAT(name, prot) creat(name, prot)
#define	DF_MT   DFMT_ALLIANT
#endif /*ALLIANT*/


#ifdef IRIS4
#undef DF_STRUCTOK
#include <sys/types.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
typedef	short int16;
typedef	unsigned short uint16;
typedef	long int32;
typedef	float float32;
typedef double float64;
#define	DFmovmem(from, to, len) bcopy(from, to, len)
#ifndef DF_STRUCTOK
#define	UINT16READ(p, x)    { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x)     { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x)     { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
                                x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x)   { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x)    { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x)    { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;   \
                                *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_IRIS4
#endif /*IRIS4*/


#ifdef MIPSEL
#undef DF_STRUCTOK
#include <sys/types.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
typedef short int16;
typedef unsigned short uint16;
typedef long int32;
typedef float float32;
typedef double float64;
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x)    { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x)     { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x)     { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
                              x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x)   { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x)    { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x)    { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
                              *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_MIPSEL
#endif /*MIPSEL*/

#ifdef RS6000
#undef DF_STRUCTOK
#include <sys/types.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
typedef short int16;
typedef unsigned short uint16;
typedef long int32;
typedef float float32;
typedef double float64;
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x)    { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x)     { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x)     { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
                              x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x)   { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x)    { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x)    { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
                              *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_SUN
#endif /*RS6000*/

#ifdef MAC
#undef DF_BUFFIO		/* use unbuffered i/o */
#include <memory.h>             /* malloc stuff for MPW 3.0 */
#include <fcntl.h>              /* unbuffered IO stuff for MPW 3.0 */
#ifdef THINK_C                  /* for LightSpeed C */
#include <unix.h>
#else /*THINK_C                   MPW, possibly others */
#include <Files.h>              /* for unbuffered i/o stuff */
#endif /*THINK_C*/
#define	DF_CAPFNAMES            /* fortran names are in all caps */
#define DF_DYNAMIC		/* use dynamic allocation */
typedef short int16;
typedef unsigned short uint16;
typedef long int32;
typedef float float32;
typedef double float64;
#ifdef THINK_C                   /* LightSpeed C does not have memcpy */
#define DFmovmem(from, to, len) DFImemcopy(from, to, len)
#else /*THINK_C*/
#define DFmovmem(from, to, len) memcpy(to, from, len)
#endif /*THINK_C*/
#define malloc(x)   NewPtr((Size)   (x))    /* don't use malloc on the Mac */
#define free(x)     DisposPtr((Ptr) (x))    /* don't use free on the Nac   */ 
#undef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#define DF_CREAT(name, prot) mopen(name, O_WRONLY|O_TRUNC|O_CREAT)
#define DF_MT   DFMT_MAC
#endif /*MAC*/

#ifdef VMS
/*#undef DF_BUFFIO should be buff !!!!*/
   /* use only unbuff i/o - buff doesn't work! */
#ifndef DFopen                  /* avoid double includes */
#include "dfivms.h"
#endif /*DFopen*/
#undef DF_STRUCTOK
#define DF_CAPFNAMES            /* fortran names are in all caps */
#include <file.h>               /* for unbuffered i/o stuff */
typedef short int16;
typedef unsigned short uint16;
typedef long int32;
typedef float float32;
typedef double float64;
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_VAX
#endif /*VMS*/

#ifdef APOLLO
#if ! defined mc68010 && ! defined mc68020 && ! defined mc68030
#undef DF_STRUCTOK
#endif
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define int16 short
#define uint16 unsigned short
#define int32 long
#define float32 float
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_APOLLO
#endif /*APOLLO*/


/*--------------------------------------------------------------------------*/
/*                      Flexibility parameters                              */
#ifdef MAC			/* MAC specific file manager calls */
#	define DF_OPEN(x,y) mopen(x,y)
#	define DF_CLOSE(x) mclose(x)
#	define DF_SEEK(x,y,z) mlseek(x,y,z)
#	define DF_SKEND(x,y,z) mlseek(x,-1*y,z)
#	define DF_TELL(x) mlseek(x,0L,1)
#	define DF_READ(a,b,c,d) mread(d,a,b*c)
#	define DF_WRITE(a,b,c,d) mwrite(d,a,b*c)
#	define DF_FLUSH(a)			/* no need to flush */
#	define DF_RDACCESS 0		/* dummy */
#	define DF_WRACCESS 0		/* dummy */
#	define DF_OPENERR(f)	((f) == -1)
#else /* !MAC */
#ifdef DF_BUFFIO            /* set all calls to do buffered I/O */
#define DF_OPEN(x,y) fopen(x,y)
#define DF_CLOSE(x) fclose(x)
#define DF_SEEK(x,y,z) fseek(x,y,z)
#define DF_SKEND(x,y,z) fseek(x,y,z)
#define DF_TELL(x) ftell(x)
#define DF_READ(a,b,c,d) fread(a,b,c,d)
#define DF_WRITE(a,b,c,d) fwrite(a,b,c,d)
#define DF_FLUSH(a) fflush(a)
#define DF_OPENERR(f)	(!(f))
#ifdef PC
#define DF_RDACCESS "rb"
#define DF_WRACCESS "rb+"
#else /*PC*/
#define DF_RDACCESS "r"
#define DF_WRACCESS "r+"
#endif /*PC*/

#else /*DF_BUFFIO         unbuffered i/o */
#define DF_OPEN(x,y) open(x,y)
#define DF_CLOSE(x) close(x)
#define DF_SEEK(x,y,z) lseek(x,y,z)
#define DF_SKEND(x,y,z) lseek(x,-1*y,z)
#define DF_TELL(x) lseek(x,0L,1)
#define DF_READ(a,b,c,d) read(d,a,b*c)
#define DF_WRITE(a,b,c,d) write(d,a,b*c)
#define DF_OPENERR(f)	((f) == -1)
#define DF_FLUSH(a)                             /* no need to flush */
#  ifdef PC
#    define DF_RDACCESS O_RDONLY | O_RAW
#    define DF_WRACCESS O_RDWR | O_RAW
#  else 
#    define DF_RDACCESS O_RDONLY
#    define DF_WRACCESS O_RDWR
#  endif /*!PC*/
#endif /*DF_BUFFIO*/
#endif /* !MAC */


    /* if not allocating memory dynamically, need buffer for compression */
#ifndef DF_DYNAMIC
#define DF_TBUF
#define DF_TBUFSZ	10000	/* buffer size */
#endif /*DF_DYNAMIC*/

    /* if reading/writing structures not ok, need buffer for conversion */
#ifndef DF_TBUF
#ifndef DF_STRUCTOK
#define DF_TBUF
#define DF_TBUFSZ	512	/* buffer size can be smaller */
#endif /*DF_STRUCTOK*/
#endif /*DF_TBUF*/

    /* set buffer size */
#ifdef DF_TBUF
#ifndef DFMASTER
extern
#endif /*DFMASTER*/
    char DFtbuf[DF_TBUFSZ];
#endif /*DF_TBUF*/

/* VMS str descriptor conversion macro */
#if defined(VMS)
#   define _fcdtocp(desc) ((char *) *((char **) &desc[4]))
    typedef char *_fcd;
#else /* !VMS */
#if defined(UNICOS)
#   include <fortran.h>
#else /* !VMS && !UNICOS */
#if defined(MAC)		/* with LS_FORTRAN */
#   define _fcdtocp(descp) (*((char**)(descp)))
    typedef char** _fcd;
#else /* !VMS && !UNICOS && !MAC */
#   define _fcdtocp(desc) (desc)
    typedef char *_fcd;
#endif
#endif /* !VMS && !UNICOS */
#endif /* !VMS */

/* 
MACRO FCALLKEYW for any special fortran-C stub keyword

MacIntosh MPW LS-fortran needs pascal since it can interface best with
pascal functions
*/
#if defined(MAC)		/* with LS FORTRAN */
#   define FCALLKEYW	pascal
#else /* !MAC */
#   define FCALLKEYW	/*NONE*/
#endif

/*--------------------------------------------------------------------------*/
/*                          Size parameters                                 */
#define DF_MAXDFS           32  /* How many DF's can be open at once */
#define DF_DEFAULTDDS       16  /* How many DD's a file has by default */
#define DF_MAXFNLEN         256 /* maximum length of filename parameters */

#ifndef MAC
extern	char *strncpy();
#ifndef	RS6000
extern	char *strcpy();
#endif
extern	char *malloc();
#if	defined(sgi) || defined(MIPSEL) || defined(UNICOS) || defined(hpux) || defined(RS6000)
extern	void *memcpy();
#else
extern	char *memcpy();
#endif
#endif /* !MAC */

#endif /*DF_MAGICK*/
