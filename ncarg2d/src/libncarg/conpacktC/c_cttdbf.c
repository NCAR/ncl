/*
 *      $Id: c_cttdbf.c,v 1.3 2008-07-23 16:16:44 haley Exp $
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

extern void NGCALLF(cttdbf,CTTDBF)(float*,int*,int*,float*,int*,int*,float*);

void c_cttdbf
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int iflg,
    float atol
)
#else
(rpnt,iedg,itri,rwrk,iwrk,iflg,atol)
    float *rpnt;
    int *iedg;
    int *itri;
    float *rwrk;
    int *iwrk;
    int iflg;
    float atol;
#endif
{
    NGCALLF(cttdbf,CTTDBF)(rpnt,iedg,itri,rwrk,iwrk,&iflg,&atol);
}
