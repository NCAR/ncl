/*
 * $Id: nnuhead.h,v 1.12 2008-07-27 04:02:37 haley Exp $
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

#include <ncarg/ngmath.h>

#define EQ             ==
#define NE             !=
#define AND            &&
#define OR             ||

extern int       igrad,  densi,   non_neg,    sdip,        rads,
                 optim,  extrap,  southhemi,  updir, auto_scale,
                 adf,    nndup,   single_point, maxmsg;

extern double    bI,        bJ,         magx,       magy,
                 magz,      horilap,    vertlap,    nuldat,
                 magx_auto, magy_auto,  magz_auto,  horilap_save,
                 vertlap_save;

extern char      tri_file[], error_file[], emsg[];

extern void   ErrorHnd(int, char *, FILE *, char *);
