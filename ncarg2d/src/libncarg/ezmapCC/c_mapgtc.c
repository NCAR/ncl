/*
 *	$Id: c_mapgtc.c,v 1.1 2001-10-10 02:51:18 haley Exp $
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

extern void NGCALLF(mapgtc,MAPGTC)(NGstring,NGstring,int,int);

void c_mapgtc
#ifdef NeedFuncProto
(
    char *whch,
    char *cval,
    int len
)
#else
(whch,cval,len)
    char *whch;
    char *cval;
    int len;
#endif
{
    int i, len1;
    char error_msg[256];
    NGstring whch2;
    NGstring cval2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cval, len, error_msg ) ) {
        fprintf( stderr, "c_mapgtc:  %s\n", error_msg );
        return;
    }
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) { 
        fprintf( stderr, "c_mapgtc:  illegal parameter string (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len1);
    cval2 = NGCstrToFstr(cval,len);
    NGCALLF(mapgtc,MAPGTC)(whch2,cval2,len1,len);

    cval = NGFstrToCstr(cval2);
    cval[len] = '\0';
    for( i = len-1; i >= 0; i-- ) {
        if( cval[i] != ' ' && cval[i] != '\0' ) {
            cval[i+1] = '\0';
            break;
        }
    }
}
