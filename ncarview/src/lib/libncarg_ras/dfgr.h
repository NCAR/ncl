/*
 *	$Id: dfgr.h,v 1.2 1991-08-16 11:09:26 clyne Exp $
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

/*
$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libncarg_ras/Attic/dfgr.h,v 1.2 1991-08-16 11:09:26 clyne Exp $

$Log: dfgr.h,v $
Revision 1.2  1991-08-16 11:09:26  clyne
*** empty log message ***

 * Revision 1.1  91/06/18  15:07:44  clyne
 * Initial revision
 * 
 * Revision 1.1.1.1  90/12/11  14:23:53  clyne
 * hdf 3.10r3 distribution
 * 
 * Revision 1.1  90/12/11  14:23:52  clyne
 * Initial revision
 * 
 * Revision 3.0  90/02/02  20:34:28  clow
 * *** empty log message ***
 * 
*/
/*****************************************************************************
* 
*			  NCSA HDF version 3.00
*				December, 1989
*
* NCSA HDF Version 3.00 source code and documentation are in the public
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

/*-----------------------------------------------------------------------------
 * File:    dfgr.h
 * Purpose: header file for the Raster Image set
 * Invokes: df.h
 * Contents: 
 *  Structure definitions: DFGRdr, DFGRrig
 * Remarks: This is included with user programs which use general raster
 *---------------------------------------------------------------------------*/


#ifndef DFGR                        /* avoid re-inclusion */
#define DFGR

#include "df.h"

/* description record: used to describe image data, palette data etc. */
typedef struct {
    int32 xdim, ydim;               /* dimensions of data */
    DFdi nt;                        /* number type of data */
    int16 ncomponents, interlace;   /* data ordering: chunky / planar etc */
    DFdi compr;                     /* compression */
        /* ### Note: compression is currently uniquely described with a tag.
            No data is attached to this tag/ref.  But this capability is
            provided for future expansion, when this tag/ref might point to
            some data needed for decompression, such as the actual encodings */
} DFGRdr;

/* structure to hold RIG info */
typedef struct {
    DFdi data[3];                   /* image/lut/mattechannel */
    DFGRdr datadesc[3];             /* description of image/lut/mattechannel */
    int32 xpos, ypos;               /* X-Y position of image on screen */
    float aspectratio;              /* ratio of pixel height to width */
    float ccngamma,                 /* color correction parameters */
        ccnred[3], ccngrren[3], ccnblue[3], ccnwhite[3];
    char *cf;                       /* color format */
} DFGRrig;

#endif /*DFGR*/
