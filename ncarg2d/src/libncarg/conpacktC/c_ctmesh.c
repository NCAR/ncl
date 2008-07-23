/*
 *      $Id: c_ctmesh.c,v 1.2 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(ctmesh,CTMESH)(float*,int*,int*,
                                     int*,int*,int*,
                                     int*,int*,int*,
                                   float*,int*,
                                     int*,int*);

void c_ctmesh
#ifdef NeedFuncProto
(
    float *rpnt,
    int npnt,
    int lopn,
    int *iedg,
    int nedg,
    int loen,
    int *itri,
    int ntri,
    int lotn,
    float *rwrk,
    int lrwk,
    int *iwrk,
    int liwk
)
#else
(rpnt,npnt,lopn,iedg,nedg,loen,itri,ntri,lotn,rwrk,lrwk,iwrk,liwk)
    float *rpnt;
    int npnt;
    int lopn;
    int *iedg;
    int nedg;
    int loen;
    int *itri;
    int ntri;
    int lotn;
    float *rwrk;
    int lrwk;
    int *iwrk;
    int liwk;
#endif
{
    NGCALLF(ctmesh,CTMESH)(rpnt,&npnt,&lopn,
                           iedg,&nedg,&loen,
                           itri,&ntri,&lotn,
                           rwrk,&lrwk,
                           iwrk,&liwk);
}
