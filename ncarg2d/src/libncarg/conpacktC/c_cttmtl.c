/*
 *      $Id: c_cttmtl.c,v 1.2 2008-07-23 16:16:45 haley Exp $
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

#include <ncarg/ncargC.h>

extern void NGCALLF(cttmtl,CTTMTL)(int*,float*,int*,int*,
                                   int*,int*,int*,
                                   int*,int*,int*,
                                   float*,int*,int*,int*,
                                     int*,int*,int*,int*,
                                     int*,int*,int*,int*);

void c_cttmtl
#ifdef NeedFuncProto
(
    int ntto,
    float *tbuf,
    int mbuf,
    int *nbuf,
    int *ippp,
    int mppp,
    int *nppp,
    int *ippe,
    int mppe,
    int *nppe,
    float *rpnt,
    int mpnt,
    int *npnt,
    int lopn,
    int *iedg,
    int medg,
    int *nedg,
    int loen,
    int *itri,
    int mtri,
    int *ntri,
    int lotn
)
#else
(ntto,tbuf,mbuf,nbuf,ippp,mppp,nppp,ippe,mppe,nppe,rpnt,mpnt,npnt,lopn,
 iedg,medg,nedg,loen,itri,mtri,ntri,lotn)
    int ntto;
    float *tbuf;
    int mbuf;
    int *nbuf;
    int *ippp;
    int mppp;
    int *nppp;
    int *ippe;
    int mppe;
    int *nppe;
    float *rpnt;
    int mpnt;
    int *npnt;
    int lopn;
    int *iedg;
    int medg;
    int *nedg;
    int loen;
    int *itri;
    int mtri;
    int *ntri;
    int lotn;
#endif
{
    NGCALLF(cttmtl,CTTMTL)(&ntto,tbuf,&mbuf,nbuf,
                           ippp,&mppp,nppp,
                           ippe,&mppe,nppe,
                           rpnt,&mpnt,npnt,&lopn,
                           iedg,&medg,nedg,&loen,
                           itri,&mtri,ntri,&lotn);
}
