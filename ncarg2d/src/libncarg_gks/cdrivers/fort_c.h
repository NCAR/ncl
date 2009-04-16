/*
 *      $Id: fort_c.h,v 1.10 2009-04-16 06:51:33 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *      File:           fort_c.h
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri May  3 11:51:44 MDT 1991
 *
 *      Description:    Header file for Fortran/C interface.
 *              
 *      
 *
 */
#ifndef _fort_c_
#define _fort_c_

#include <ncarg/c.h>
/*
 *      supported output device identifiers
 */
#define DEV_CGM         1       /* not supported in this directory   */
#define DEV_WISS        3       /* not supported in this directory   */
#define DEV_X11P        7       /* X11 private          */
#define DEV_X11         8       /* X11 regular          */
#define DEV_PIX         9       /* pixmap               */
#define DEV_CTXT       10
#define DEV_PDF_P      11       /* PDF portrait */
#define DEV_PDF_L      12       /* PDF landscape */
#define DEV_PS         20       /* generic id for all PS drivers */
#define DEV_PS_MIN     20       /* smallest id for the PS drivers */
#define DEV_PS_MAX     31       /* largest id for the PS drivers */
#define DEV_CRO        40       /* generic id for the cairo drivers */
#define DEV_CRO_MIN    40       /* smallest id for the cairo drivers */
#define DEV_CRO_MAX    51       /* largest id for the cairo drivers */


#define ERR_MSG_MAX     160     /* maximum error message size   */

#endif  /*      _fort_c_         */
