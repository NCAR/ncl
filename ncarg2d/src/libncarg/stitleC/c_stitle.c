/*
 *	$Id: c_stitle.c,v 1.3 2000-07-12 16:26:03 haley Exp $
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

void c_stitle
#ifdef NeedFuncProto
(
    char *crds[],
    int ncds,
    int iyst,
    int iynd,
    float tmst,
    float tmmv,
    float tmnd,
    int mtst
)
#else
(crds,ncds,iyst,iynd,tmst,tmmv,tmnd,mtst)
    char *crds[];
    int ncds;
    int iyst;
    int iynd;
    float tmst;
    float tmmv;
    float tmnd;
    int mtst;
#endif
{
    float tmst2,tmmv2,tmnd2;
    int i, len, maxlen;
    char *crds2;
    NGstring crds22;

    maxlen = strlen(crds[0]);
    for( i = 1; i < ncds; i++ ) {
	len = strlen(crds[i]);
        maxlen = len > maxlen ? len : maxlen;
    }
    crds2 = (char *)malloc((maxlen*ncds+1)*sizeof(char));
    if( crds2 == NULL ) {
	printf( "\nc_stitle:  Unable to create memory for array crds2\n" );
        return;
    }
    Pad_char_array(crds,crds2,ncds,maxlen);
    tmst2 = tmst;
    tmmv2 = tmmv;
    tmnd2 = tmnd;
    crds22 = NGCstrToFstr(crds2,maxlen);
    NGCALLF(stitle,STITLE)(crds22,&ncds,&iyst,&iynd,&tmst2,&tmmv2,&tmnd2,
			   &mtst,maxlen);
    free((char *) crds2);
}
