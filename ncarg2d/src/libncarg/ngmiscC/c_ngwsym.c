/*
 *	$Id: c_ngwsym.c,v 1.2 2000-07-12 16:24:51 haley Exp $
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

void c_ngwsym
#ifdef NeedFuncProto
(
    char *ftype,
    int num,
    float x,
    float y,
    float size,
    int icolor,
    int ialt
)
#else
(ftype,num,x,y,size,icolor,ialt)
    char *ftype;
    int num;
    float x;
    float y;
    float size;
    int icolor;
    int ialt;
#endif
{
    float x2, y2, size2;
    NGstring ftype2;
    int len;
/*
 * Make sure font parameter is not NULL
 */
    if( !ftype ) {
        fprintf( stderr, "c_ngwsym:  illegal font parameter name (NULL)\n" );
        return;
    }

    x2 = x;
    y2 = y;
    size2 = size;

    len = NGSTRLEN(ftype);
    ftype2 = NGCstrToFstr(ftype,len);
    NGCALLF(ngwsym,NGWSYM)(ftype2,&num,&x2,&y2,&size2,&icolor,&ialt,len);
}
