/*
 *	$Id: c_ardrln.c,v 1.5 2008-07-23 16:16:39 haley Exp $
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

extern void NGCALLF(ardrln,ARDRLN)(int*,float*,float*,int*,float*,float*,
                                   int*,int*,int*,int*,int (*colrln_)());


void c_ardrln
#ifdef NeedFuncProto
(
    int *iam,
    float *xcd,
    float *ycd,
    int ncd,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*colrln_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
               )
)
#else
(iam,xcd,ycd,ncd,xcs,ycs,mcs,iai,iag,mai,colrln_)
    int *iam;
    float *xcd;
    float *ycd;
    int ncd;
    float *xcs;
    float *ycs;
    int mcs;
    int *iai;
    int *iag;
    int mai;
    int (*colrln_)();
#endif
{
    NGCALLF(ardrln,ARDRLN)(iam,xcd,ycd,&ncd,xcs,ycs,&mcs,iai,iag,&mai,colrln_);
}
