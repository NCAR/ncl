/*
 *	$Id: c_nggcog.c,v 1.3 2000-07-31 20:11:34 haley Exp $
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

extern void NGCALLF(nggcog,NGGCOG)(float*,float*,float*,float*,float*,int*);

void c_nggcog
#ifdef NeedFuncProto
(
    float clat,
    float clon,
    float crad,
    float *alat,
    float *alon,
    int npts
)
#else
(clat,clon,crad,alat,alon,npts)
    float clat;
    float clon;
    float crad;
    float *alat;
    float *alon;
    int npts;
#endif
{
    NGCALLF(nggcog,NGGCOG)(&clat,&clon,&crad,alat,alon,&npts);
}
