/*
 *	$Id: c_ngpswk.c,v 1.6 2008-07-23 16:16:58 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdlib.h>

#include <ncarg/ncargC.h>

extern int NGCALLF(ngpswk,NGPSWK)(NGstring,NGstring,NGstring,int,int,int);

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


