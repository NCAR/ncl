/*
 *      $Id: c_mdrgsf.c,v 1.3 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdrgsf,MDRGSF)(int*,float*,int*,int*,int*);

void c_mdrgsf
#ifdef NeedFuncProto
(
    int irgl,
    float *rwrk,
    int lrwk,
    int *iama,
    int lama
)
#else
(irgl)
    int irgl;
    float *rwrk;
    int lrwk;
    int *iama;
    int lama;
#endif
{
    NGCALLF(mdrgsf,MDRGSF)(&irgl,rwrk,&lrwk,iama,&lama);
}
