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
static char RcsId[] = "@(#)$Revision: 1.1 $"
#endif
/*
$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libncarg_ras/Attic/df.h,v 1.1 1991-06-18 15:11:16 clyne Exp $
$Log: df.h,v $
Revision 1.1  1991-06-18 15:11:16  clyne
Initial revision

 * Revision 1.1.1.1  90/12/11  14:22:37  clyne
 * hdf 3.10r3 distribution
 * 
 * Revision 1.1  90/12/11  14:22:35  clyne
 * Initial revision
 * 
 * Revision 3.4  90/11/28  08:12:26  mfolk
 * *** empty log message ***
 * 
 * Revision 3.3  90/07/19  10:52:32  clow
 * added DFTAG_IMCOMP as alias for DFTAG_IMC
 * 
 * Revision 3.2  90/05/17  17:12:32  clow
 * removed #include <fortran.h> which has migrated to dfi.h
 * 
*/
/*-----------------------------------------------------------------------------
 * File:    df.h
 * Purpose: header file for HDF routines
 * Invokes: dfi.h
 * Contents: 
 *  Structure definitions: DFddh, DFdd, DFdesc, DFdle, DF, DFdi, DFdata
 *  Procedure type definitions
 *  Global variables
 *  Tag definitions
 *  Error return codes
 *  Logical constants
 * Remarks: This file is included with user programs
 *          Since it includes stdio.h etc., do not include these after df.h
 *---------------------------------------------------------------------------*/


#ifndef DFTAG_NULL              /* avoid re-inclusion */

/* include DF (internal) header information */
#include "dfi.h"

/*-------------------------------------------------------------------------*/
/*                      Type declarations                                   */

typedef struct DFddh {		/*format of data descriptor headers in file*/
    int16 dds;			/* number of dds in header block */
    int32 next;			/* offset of next header block */
} DFddh;

typedef struct DFdd {		/* format of data descriptors as in file */
    uint16 tag;			/* data tag */
    uint16 ref;			/* data reference number */
    int32 offset;		/* offset of data element in file */
    int32 length;		/* number of bytes */
} DFdd;

/* descriptor structure is same as dd structure.  ###Note: may be changed */
#define DFdesc DFdd

/* DLE is the internal structure which stores data descriptor information */
/* It is a linked list of DDs */
typedef struct DFdle {		/* Data List element */
    struct DFdle *next;		/* link to next dle */
    DFddh ddh;			/* To store headers */
    DFdd dd[1];			/* dummy size */
} DFdle;

/* DF is the internal structure associated with each DF file */
/* It holds information associated with the file as a whole */
/* ### Note: there are hooks for having multiple DF files open at a time */
typedef struct DF {
    DFdle *list;		/* Pointer to the DLE list */
    DFdle *last_dle;		/* last_dle and last_dd are used in searches */
				/* to indicate element returned */
				/* by previous call to DFfind */
    int type;			/* 0= not in use, 1= normal, -1 = multiple */
				/* this is a hook for when */
				/* multiple files are open */
    int access;			/* permitted access types: */
				/* 0=none, 1=r, 2=w, 3=r/w */
    int changed;		/* True if anything in DDs modified */
				/* since last write */
    uint16 last_tag;		/* Last tag searched for by DFfind */
    uint16 last_ref;		/* Last reference number searched for */
    int last_dd;		/* see last_dle */
    int defdds;			/* default numer of DD's in each block */
    int up_access;		/* access permissions to element being */
				/* read/updated. Used by DFstart */
    DFdd *up_dd;		/* DD of element being read/updated, */
				/* used by DFstart */
    /* file handle is a file pointer or file descriptor depending on whether */
    /* we use buffered or unbuffered i/o */
#ifdef DF_BUFFIO
    FILE *file;			/* file pointer */
#else /*DF_BUFFIO*/
    int file;			/* file descriptor */
#endif /*DF_BUFFIO*/
} DF;


typedef struct DFdi {   /* data identifier: specifies data element uniquely */
    uint16 tag;
    uint16 ref;
} DFdi;



typedef struct DFdata { /* structure for returning status information */
    int version;        /* version number of program */
} DFdata;

/*--------------------------------------------------------------------------*/
/*                          Procedure types                                 */

#ifndef VMS
DF *DFopen();
int32 DFgetelement(), DFread(), DFseek(), DFwrite();
uint16 DFnewref();
char *DFIgetspace(), *DFIfreespace(), *DFIf2cstring();
#else /*VMS*/
            /* for VMS need to prepend _ to avoid name conflict with Fortran */
DF *_DFopen();
int32 _DFgetelement(), _DFread(), _DFseek(), _DFwrite();
uint16 _DFnewref();
char *_DFIgetspace(), *_DFIfreespace(), *_DFIf2cstring();
#endif /*VMS*/

/*--------------------------------------------------------------------------*/
/*                          Global Variables                                */

#ifndef DFMASTER
extern
#endif /*DFMASTER*/
int DFerror;            /* Error code for DF routines */

/*--------------------------------------------------------------------------*/
/*                           Tag Definitions                                */

#define DFREF_WILDCARD ((uint16)0) /* wildcard ref for searches */

#define DFTAG_WILDCARD ((uint16)0) /* wildcard tag for searches */
#define DFTAG_NULL  ((uint16)1)	/* empty DD */

/* utility set */
#define DFTAG_FID   ((uint16)100) /* File identifier */
#define DFTAG_FD    ((uint16)101) /* File description */
#define DFTAG_TID   ((uint16)102) /* Tag identifier */
#define DFTAG_TD    ((uint16)103) /* Tag descriptor */
#define DFTAG_DIL   ((uint16)104) /* data identifier label */
#define DFTAG_DIA   ((uint16)105) /* data identifier annotation */
#define DFTAG_NT    ((uint16)106) /* number type */
#define DFTAG_MT    ((uint16)107) /* machine type */

/* raster-8 set */
#define DFTAG_ID8   ((uint16)200) /* 8-bit Image dimension */
#define DFTAG_IP8   ((uint16)201) /* 8-bit Image palette */
#define DFTAG_RI8   ((uint16)202) /* Raster-8 image */
#define DFTAG_CI8   ((uint16)203) /* RLE compressed 8-bit image */
#define DFTAG_II8   ((uint16)204) /* IMCOMP compressed 8-bit image */

/* Raster Image set */
#define DFTAG_ID    ((uint16)300) /* Image DimRec */
#define DFTAG_LUT   ((uint16)301) /* Image Palette */
#define DFTAG_RI    ((uint16)302) /* Raster Image */
#define DFTAG_CI    ((uint16)303) /* Compressed Image */

#define DFTAG_RIG   ((uint16)306) /* Raster Image Group */
#define DFTAG_LD    ((uint16)307) /* Palette DimRec */
#define DFTAG_MD    ((uint16)308) /* Matte DimRec */
#define DFTAG_MA    ((uint16)309) /* Matte Data */
#define DFTAG_CCN   ((uint16)310) /* color correction */
#define DFTAG_CFM   ((uint16)311) /* color format */
#define DFTAG_AR    ((uint16)312) /* aspect ratio */

#define DFTAG_DRAW  ((uint16)400) /* Draw these images in sequence */
#define DFTAG_RUN   ((uint16)401) /* run this as a program/script */

#define DFTAG_XYP   ((uint16)500) /* x-y position */
#define DFTAG_MTO   ((uint16)501) /* machine-type override */

/* Tektronix */
#define DFTAG_T14   ((uint16)602) /* TEK 4014 data */
#define DFTAG_T105  ((uint16)603) /* TEK 4105 data */

/* Scientific Data set */
#define DFTAG_SDG   ((uint16)700) /* Scientific Data Group */
#define DFTAG_SDD   ((uint16)701) /* Scientific Data DimRec */
#define DFTAG_SD    ((uint16)702) /* Scientific Data */
#define DFTAG_SDS   ((uint16)703) /* Scales */
#define DFTAG_SDL   ((uint16)704) /* Labels */
#define DFTAG_SDU   ((uint16)705) /* Units */
#define DFTAG_SDF   ((uint16)706) /* Formats */
#define DFTAG_SDM   ((uint16)707) /* Max/Min */
#define DFTAG_SDC   ((uint16)708) /* Coord sys */
#define DFTAG_SDT   ((uint16)709) /* Transpose */

/* compression schemes */
#define DFTAG_RLE   ((uint16)11) /* run length encoding */
#define DFTAG_IMC   ((uint16)12) /* IMCOMP compression */
#define DFTAG_IMCOMP   ((uint16)12) /* IMCOMP compression */

/*--------------------------------------------------------------------------*/
/*                          Error Return Codes                              */

#define DFE_NOERROR     0   /* No error */
#define DFE_FNF         -1  /* File not found error */
#define DFE_DENIED      -2  /* Access to file denied */
#define DFE_ALROPEN     -3  /* File already open */
#define DFE_TOOMANY     -4  /* Too Many DF's or files open */
#define DFE_BADNAME     -5  /* Bad file name on open */
#define DFE_BADACC      -6  /* Bad file access mode */
#define DFE_BADOPEN     -7  /* Other open error */
#define DFE_NOTOPEN     -8  /* File can't be closed 'cause it isn't open */
#define DFE_CANTCLOSE   -9  /* fclose wouldn't work! */
#define DFE_DFNULL      -10 /* DF is a null pointer */
#define DFE_ILLTYPE     -11 /* DF has an illegal type: internal error */
#define DFE_UNSUPPORTED -12 /* Feature not currently supported */
#define DFE_BADDDLIST   -13 /* The DD list is non-existent: internal error */
#define DFE_NOTDFFILE   -14 /* This is not a DF file and it is not 0 length */
#define DFE_SEEDTWICE   -15 /* The DD list already seeded: internal error */
#define DFE_NOSPACE     -16 /* Malloc failed */
#define DFE_NOSUCHTAG   -17 /* There is no such tag in the file: search failed*/
#define DFE_READERROR   -18 /* There was a read error */
#define DFE_WRITEERROR  -19 /* There was a write error */
#define DFE_SEEKERROR   -20 /* There was a seek error */
#define DFE_NOFREEDD    -21 /* There are no free DD's left: internal error */
#define DFE_BADTAG      -22 /* illegal WILDCARD tag */
#define DFE_BADREF      -23 /* illegal WILDCARD reference # */
#define DFE_RDONLY      -24 /* The DF is read only */
#define DFE_BADCALL     -25 /* Calls in wrong order */
#define DFE_BADPTR      -26 /* NULL ptr argument */
#define DFE_BADLEN      -27 /* negative len specified */
#define DFE_BADSEEK     -28 /* Attempt to seek past end of element */
#define DFE_NOMATCH     -29 /* No (more) DDs which match specified tag/ref */
#define DFE_NOTINSET    -30 /* Warning: Set contained unknown tag: ignored */
#define DFE_BADDIM      -31 /* negative or zero dimensions specified */
#define DFE_BADOFFSET   -32 /* Illegal offset specified */
#define DFE_BADSCHEME   -33 /* Unknown compression scheme specified */
#define DFE_NODIM       -34 /* No dimension record associated with image */
#define DFE_NOTENOUGH   -35 /* space provided insufficient for size of data */
#define DFE_NOVALS      -36 /* Values not available */
#define DFE_CORRUPT     -37 /* File is corrupted */
#define DFE_BADCONV     -37 /* Don't know how to convert data type */
#define DFE_BADFP       -38 /* The file contained an illegal floating point no*/
#define DFE_NOREF       -39 /* no more reference numbers are available */
#define DFE_BADDATATYPE -40 /* unknown or unavailable data type specified */
#define DFE_BADMCTYPE   -41 /* unknown or unavailable machine type specified */
#define DFE_BADNUMTYPE  -42 /* unknown or unavailable number type specified */
#define DFE_BADORDER    -43 /* unknown or illegal array order specified */
#define DFE_NOTIMPL     -44 /* This feature not yet implemented */

/*--------------------------------------------------------------------------*/
/*                          Logical Constants                               */

#define DFACC_READ      1   /* Read Access */
#define DFACC_WRITE     2   /* Write Access */
#define DFACC_CREATE    4   /* force file to be created */
#define DFACC_ALL       7   /* the logical and of all the above values */

/*--------------------------------------------------------------------------*/
/*                          Settype Constants                               */


#endif /*DFTAG_NULL*/
