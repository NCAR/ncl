/*
 *	$Id: c_dpsetc.c,v 1.3 2000-07-31 20:11:09 haley Exp $
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

extern void NGCALLF(dpsetc,DPSETC)(NGstring,NGstring,int,int);

void c_dpsetc
#ifdef NeedFuncProto
(
    char *pnam,
    char *cval
)
#else
( pnam, cval )
    char *pnam;
    char *cval;
#endif
{
    NGstring pnam2;
    NGstring cval2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_dpsetc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(pnam);
    len2 = NGSTRLEN(cval);
    pnam2 = NGCstrToFstr(pnam,len1);
    cval2 = NGCstrToFstr(cval,len2);
    NGCALLF(dpsetc,DPSETC)(pnam2,cval2,len1,len2);
}


