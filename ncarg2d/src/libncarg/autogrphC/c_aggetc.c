/*
 *	$Id: c_aggetc.c,v 1.3 2000-07-12 16:22:06 haley Exp $
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

void c_aggetc
#ifdef NeedFuncProto
(
    char *tpid,
    char *cusr,
    int len
)
#else
(tpid,cusr,len)
    char *tpid;
    char *cusr;
    int len;
#endif
{
    int i, len1;
    char error_msg[256];

    NGstring tpid2;
    NGstring cusr2;
/* 
 *  Make sure return string is valid
 */
    if( chk_ret_str( cusr, len, error_msg ) ) {
        fprintf( stderr, "c_aggetc:  %s\n", error_msg );
        return;
    }
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggetc:  illegal parameter string (NULL)\n" );
        return;
    }

    len1 = NGSTRLEN(tpid);
    cusr2 = NGCstrToFstr(cusr,len);
    tpid2 = NGCstrToFstr(tpid,len1);
    NGCALLF(aggetc,AGGETC)(tpid2,cusr2,len1,len-1);

    cusr = NGFstrToCstr(cusr2);
    cusr[len-1] = '\0';
    for( i = len-2; i >= 0; i-- ) {
        if( cusr[i] != ' ' && cusr[i] != '\0' ) {
            cusr[i+1] = '\0';
            break;
        }
    }
}
