/*
 *      $Id: c_cttdbf.c,v 1.2 2004-03-27 00:14:50 kennison Exp $
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

extern void NGCALLF(cttdbf,CTTDBF)(float*,int*,int*,float*,int*,int*,float*);

void c_cttdbf
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int iflg,
    float atol
)
#else
(rpnt,iedg,itri,rwrk,iwrk,iflg,atol)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
    int iflg;
    float atol;
#endif
{
    NGCALLF(cttdbf,CTTDBF)(rpnt,iedg,itri,rwrk,iwrk,&iflg,&atol);
}
