/*
 *	$Id: c_pwrzt.c,v 1.3 2000-07-31 20:12:14 haley Exp $
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

extern void NGCALLF(pwrzt,PWRZT)(float*,float*,float*,NGstring,int*,int*,
                                 int*,int*,int*,int);

void c_pwrzt 
#ifdef NeedFuncProto
(
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
)
#else
 (x,y,z,id,n,isize,lin3,itop,icnt)
    float x;
    float y;
    float z;
    char *id;
    int n;
    int isize;
    int lin3;
    int itop;
    int icnt;
#endif
{
	NGstring id2;
    int len;
    len = NGSTRLEN(id);
	id2 = NGCstrToFstr(id,len);
    NGCALLF(pwrzt,PWRZT)(&x,&y,&z,id2,&n,&isize,&lin3,&itop,&icnt,len);
}
