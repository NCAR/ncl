/*
 *	$Id: c_encd.c,v 1.3 2000-07-31 20:12:02 haley Exp $
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

extern void NGCALLF(encd,ENCD)(float*,float*,NGstring,int*,int*,int);


void c_encd
#ifdef NeedFuncProto
(
    float valu,
    float ash,
    char *iout,
    int *nc,
    int ioffd
)
#else
(valu,ash,iout,nc,ioffd)
    float valu;
    float ash;
    char *iout;
    int *nc;
    int ioffd;
#endif
{
    NGstring iout2;
    int len;
    len = NGSTRLEN(iout);
    iout2 = NGCstrToFstr(iout,len);
    NGCALLF(encd,ENCD)(&valu,&ash,iout2,nc,&ioffd,len);
}
