/*
 *	$Id: c_gridal.c,v 1.2 2000-07-12 16:24:17 haley Exp $
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

void c_gridal
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    int ixlb,
    int iylb,
    int igph,
    float xint,
    float yint
)
#else
(mjrx,mnrx,mjry,mnry,ixlb,iylb,igph,xint,yint)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
    int ixlb;
    int iylb;
    int igph;
    float xint;
    float yint;
#endif
{
    float xint2,yint2;
    xint2 = xint;
    yint2 = yint;
    NGCALLF(gridal,GRIDAL)(&mjrx,&mnrx,&mjry,&mnry,&ixlb,&iylb,&igph,&xint2,&yint2);
}
