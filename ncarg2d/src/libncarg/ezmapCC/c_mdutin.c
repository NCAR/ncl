/*
 *      $Id: c_mdutin.c,v 1.1 2001-10-10 02:51:36 haley Exp $
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
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(mdutin,MDUTIN)(int*,int*,int*,double*,double*,double*,
				   double*,double*);

void c_mdutin
#ifdef NeedFuncProto
(
    int iprj,
    int izon,
    int isph,
    double *para,
    double umin,
    double umax,
    double vmin,
    double vmax
)
#else
(xlat,xlon,ifst,iamp,igrp,idlt,idrt)
    int iprj;
    int izon;
    int isph;
    double *para;
    double umin;
    double umax;
    double vmin;
    double vmax;
#endif
{
    NGCALLF(mdutin,MDUTIN)(&iprj,&izon,&isph,para,&umin,&umax,&vmin,&vmax);
}
