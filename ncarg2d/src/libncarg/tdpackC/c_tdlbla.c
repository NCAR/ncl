/*
 *      $Id: c_tdlbla.c,v 1.2 2000-07-12 16:26:40 haley Exp $
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

void c_tdlbla
#ifdef NeedFuncProto
(
    int   iaxs,
    char* ilbl,
    char* nlbl,
    float xat0,
    float xat1,
    float yat0,
    float yat1,
    float angd
)
#else
(iaxs,ilbl,nlbl,xat0,xat1,yat0,yat1,angd)
    int   iaxs;
    char* ilbl;
    char* nlbl;
    float xat0;
    float xat1;
    float yat0;
    float yat1;
    float angd;
#endif
{
    int iaxs2;
    int loilbl;
    NGstring ilbl2;
    int lonlbl;
    NGstring nlbl2;
    float xat02,xat12,yat02,yat12,angd2;
    iaxs2=iaxs;
    loilbl=NGSTRLEN(ilbl);
    ilbl2=NGCstrToFstr(ilbl,loilbl);
    lonlbl=NGSTRLEN(nlbl);
    nlbl2=NGCstrToFstr(nlbl,lonlbl);
    xat02=xat0;
    xat12=xat1;
    yat02=yat0;
    yat12=yat1;
    angd2=angd;
    NGCALLF(tdlbla,TDLBLA)(&iaxs2,ilbl2,nlbl2,&xat02,&xat12,&yat02,&yat12,
                                                     &angd2,loilbl,lonlbl);
}
