/*
 *	$Id: c_lblbar.c,v 1.3 2000-07-12 16:24:41 haley Exp $
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

void c_lblbar
#ifdef NeedFuncProto
(
    int ihov,
    float xleb,
    float xreb,
    float ybeb,
    float yteb,
    int nbox,
    float wsfb,
    float hsfb,
    int *lfin,
    int iftp,
    char *llbs[],
    int nlbs,
    int lbab
)
#else
(ihov,xleb,xreb,ybeb,yteb,nbox,wsfb,hsfb,lfin,iftp,llbs,nlbs,lbab)
    int ihov;
    float xleb;
    float xreb;
    float ybeb;
    float yteb;
    int nbox;
    float wsfb;
    float hsfb;
    int *lfin;
    int iftp;
    char *llbs[];
    int nlbs;
    int lbab;
#endif
{
    int i, maxlen;
    char *llbs2;
    float xleb2,xreb2,ybeb2,yteb2,wsfb2,hsfb2;
    NGstring llbs22;

    maxlen = strlen(llbs[0]);
    for( i = 1; i < nlbs; i++ ) {
        if( maxlen < strlen(llbs[i]) ) maxlen = strlen(llbs[i]);
    }
    llbs2 = (char *)malloc((maxlen*nlbs+1)*sizeof(char));
    if( llbs2 == NULL ) {
        printf( "\nc_lblbar:  Unable to create memory for array llbs2\n" );
        return;
    }
    Pad_char_array(llbs,llbs2,nlbs,maxlen);
    xleb2 = xleb;
    xreb2 = xreb;
    ybeb2 = ybeb;
    yteb2 = yteb;
    wsfb2 = wsfb;
    hsfb2 = hsfb;

    llbs22 = NGCstrToFstr(llbs2,maxlen);
    NGCALLF(lblbar,LBLBAR)(&ihov,&xleb2,&xreb2,&ybeb2,&yteb2,&nbox,
                           &wsfb2,&hsfb2,lfin,&iftp,llbs22,&nlbs,&lbab,maxlen);
    free((char *) llbs2);
}
