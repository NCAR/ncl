/*
 *	$Id: c_sfsgfa.c,v 1.5 2008-07-23 16:17:00 haley Exp $
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

extern void NGCALLF(sfsgfa,SFSGFA)(float*,float*,int*,float*,int*,int*,int*,
                                   int*);

void c_sfsgfa
#ifdef NeedFuncProto
(
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd,
    int ici
)
#else
(xra,yra,nra,dst,nst,ind,nnd,ici)
    float *xra;
    float *yra;
    int nra;
    float *dst;
    int nst;
    int *ind;
    int nnd;
    int ici;
#endif
{
    NGCALLF(sfsgfa,SFSGFA)(xra,yra,&nra,dst,&nst,ind,&nnd,&ici);
}
