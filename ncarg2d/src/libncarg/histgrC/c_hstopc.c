/*
 *	$Id: c_hstopc.c,v 1.4 2000-08-22 15:04:51 haley Exp $
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

extern void NGCALLF(hstopc,HSTOPC)(NGstring,NGstring,int*,int*,int,int);

void c_hstopc
#ifdef NeedFuncProto
(
    char *iopt,
    char *string,
    int number,
    int ilch
)
#else
(iopt,string,number,ilch)
    char *iopt;
    char *string;
    int number;
    int ilch;
#endif
{
    NGstring iopt2;
    NGstring string2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(iopt);
    len2 = NGSTRLEN(string);
    iopt2 = NGCstrToFstr(iopt,len1);
    string2 = NGCstrToFstr(string,len2);
    NGCALLF(hstopc,HSTOPC)(iopt2,string2,&number,&ilch,len1,len2);
}
