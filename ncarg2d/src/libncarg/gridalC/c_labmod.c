/*
 *	$Id: c_labmod.c,v 1.4 2000-08-22 15:04:41 haley Exp $
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

extern void NGCALLF(labmod,LABMOD)(NGstring,NGstring,int*,int*,int*,int*,
                                   int*,int*,int*,int,int);

void c_labmod
#ifdef NeedFuncProto
(
    char *fmtx,
    char *fmty,
    int numx,
    int numy,
    int iszx,
    int iszy,
    int ixdc,
    int iydc,
    int ixor
)
#else
(fmtx,fmty,numx,numy,iszx,iszy,ixdc,iydc,ixor)
    char *fmtx;
    char *fmty;
    int numx;
    int numy;
    int iszx;
    int iszy;
    int ixdc;
    int iydc;
    int ixor;
#endif
{
    NGstring fmtx2;
    NGstring fmty2;
    int len1, len2;
/*
 * Make sure label formats are not NULL
 */
    if( !fmtx || !fmty ) {
        fprintf( stderr, "c_labmod:  illegal format string (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(fmtx);
    len2 = NGSTRLEN(fmty);
    fmtx2 = NGCstrToFstr(fmtx,len1);
    fmty2 = NGCstrToFstr(fmty,len2);
    NGCALLF(labmod,LABMOD)(fmtx2,fmty2,&numx,&numy,&iszx,&iszy,&ixdc,&iydc,
                           &ixor,len1,len2);
}

