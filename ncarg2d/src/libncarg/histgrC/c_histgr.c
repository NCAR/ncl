/*
 *	$Id: c_histgr.c,v 1.2 2000-07-12 16:24:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

void c_histgr
#ifdef NeedFuncProto
(
    float *dat1,
    int ndim,
    int npts,
    int iflag,
    float *class_values,
    int nclass,
    float *wrk,
    int nwrk
)
#else
(dat1,ndim,npts,iflag,class_values,nclass,wrk,nwrk)
    float *dat1;
    int ndim;
    int npts;
    int iflag;
    float *class_values;
    int nclass;
    float *wrk;
    int nwrk;
#endif
{
    NGCALLF(histgr,HISTGR)(dat1,&ndim,&npts,&iflag,class_values,&nclass,wrk,&nwrk);
}
