/*
 * $Id: nnuhead.h,v 1.11 2005-07-29 23:19:56 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
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
