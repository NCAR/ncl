/*
 *	$Id: c_wmsetc.c,v 1.3 2000-07-31 20:12:21 haley Exp $
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

extern void NGCALLF(wmsetc,WMSETC)(NGstring,NGstring,int,int);

void c_wmsetc
#ifdef NeedFuncProto
(
    char *cnp,
    char *cvp
)
#else
( cnp, cvp )
    char *cnp;
    char *cvp;
#endif
{
    NGstring cnp2;
    NGstring cvp2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) { 
        fprintf( stderr, "c_wmsetc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(cnp);
    len2 = NGSTRLEN(cvp);
    cnp2 = NGCstrToFstr(cnp,len1);
    cvp2 = NGCstrToFstr(cvp,len2);
    NGCALLF(wmsetc,WMSETC)(cnp2,cvp2,len1,len2);
}


