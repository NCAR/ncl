/*
 *	$Id: c_q8qst4.c,v 1.2 2000-07-12 16:26:28 haley Exp $
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

void c_q8qst4
#ifdef NeedFuncProto
(
    char *name,
    char *lbrary,
    char *entry,
    char *vrsion
)
#else
(name,lbrary,entry,vrsion)
    char *name;
    char *lbrary;
    char *entry;
    char *vrsion;
#endif
{
    NGstring name2;
    NGstring lbrary2;
    NGstring entry2;
    NGstring vrsion2;
    int nlen, llen, elen, vlen;
    nlen = NGSTRLEN(name);
    llen = NGSTRLEN(lbrary);
    elen = NGSTRLEN(entry);
    vlen = NGSTRLEN(vrsion);
    name2 = NGCstrToFstr(name,nlen);
    lbrary2 = NGCstrToFstr(lbrary,llen);
    entry2 = NGCstrToFstr(entry,elen);
    vrsion2 = NGCstrToFstr(vrsion,vlen);
    NGCALLF(q8qst4,Q8QST4)(name2,lbrary2,entry2,vrsion2,nlen,llen,elen,vlen);
}
