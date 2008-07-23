/*
 *	$Id: c_stitle.c,v 1.6 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(stitle,STITLE)(NGstring,int*,int*,int*,float*,float*,
                                   float*,int*,int);

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
    crds22 = NGCstrToFstr(crds2,maxlen);
    NGCALLF(stitle,STITLE)(crds22,&ncds,&iyst,&iynd,&tmst,&tmmv,&tmnd,
			   &mtst,maxlen);
    free((char *) crds2);
}
