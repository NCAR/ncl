/*
 *      $Id: c_mdlach.c,v 1.1 2001-10-10 02:51:27 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(mdlach,MDLACH)(double*,NGstring,int*,int);

void c_mdlach
#ifdef NeedFuncProto
(
    double rlat,
    char *chrs,
    int clen,
    int *nchr
)
#else
(rlat,chrs,clen,nchr)
    double rlat;
    char *chrs;
    int clen;
    int *nchr;
#endif
{
    int i;
    char error_msg[256];
    NGstring chrs2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( chrs, clen, error_msg ) ) {
        fprintf( stderr, "c_mdlach:  %s\n", error_msg );
        return;
    }
    chrs2 = NGCstrToFstr(chrs,clen);
    NGCALLF(mdlach,MDLACH)(&rlat,chrs2,nchr,clen-1);
    chrs = NGFstrToCstr(chrs2);
    chrs[*nchr] = '\0';
}
