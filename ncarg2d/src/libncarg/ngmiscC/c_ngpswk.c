/*
 *	$Id: c_ngpswk.c,v 1.3 2000-07-12 16:24:50 haley Exp $
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

#include <stdlib.h>

#include <ncarg/ncargC.h>

int c_ngpswk
#ifdef NeedFuncProto
(
    char *pstype,
    char *orient,
    char *color
)
#else
( pstype, orient, color )
    char *pstype;
    char *orient;
    char *color;
#endif
{
    NGstring pstype2;
    NGstring orient2;
    NGstring color2;
    extern int NGCALLF(ngpswk,NGPSWK)();
    int plen, clen, olen;

    if( !orient ) {
        orient = (char *)malloc(2*sizeof(char));
        strcpy( orient, "" );
        olen = 0;
    }
    else {
        olen = strlen(orient);
    }
    if( !color ) {
        color = (char *)malloc(2*sizeof(char));
        strcpy( color, "" );
        clen = 0;
    }
    else {
        clen = strlen(color);
    }
    plen = NGSTRLEN(pstype);
    pstype2 = NGCstrToFstr(pstype,plen);
    orient2 = NGCstrToFstr(orient,olen);
    color2 = NGCstrToFstr(color,clen);
    return(NGCALLF(ngpswk,NGPSWK)(pstype2,orient2,color2,plen,olen,clen));
}


