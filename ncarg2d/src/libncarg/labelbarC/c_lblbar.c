/*
 *	$Id: c_lblbar.c,v 1.6 2008-07-23 16:16:57 haley Exp $
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

extern void NGCALLF(lblbar,LBLBAR)(int*,float*,float*,float*,float*,int*,
                                   float*,float*,int*,int*,NGstring,int*,
                                   int*,int);

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

    llbs22 = NGCstrToFstr(llbs2,maxlen);
    NGCALLF(lblbar,LBLBAR)(&ihov,&xleb,&xreb,&ybeb,&yteb,&nbox,
                           &wsfb,&hsfb,lfin,&iftp,llbs22,&nlbs,&lbab,maxlen);
    free((char *) llbs2);
}
