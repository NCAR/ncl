/*
 *	$Id: c_wmlabs.c,v 1.3 2000-07-31 20:12:21 haley Exp $
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

extern void NGCALLF(wmlabs,WMLABS)(float*,float*,NGstring,int);

void c_wmlabs
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *symtyp
)
#else
(x,y,symtyp)
    float x;
    float y;
    char *symtyp;
)
#endif
{
    NGstring symtyp2;
	int len;
	len = NGSTRLEN(symtyp);
	symtyp2 = NGCstrToFstr(symtyp,len);
    NGCALLF(wmlabs,WMLABS)(&x,&y,symtyp2,len);
}
