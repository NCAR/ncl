/*
 *	$Id: c_aredam.c,v 1.5 2008-07-23 16:16:39 haley Exp $
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

extern void NGCALLF(aredam,AREDAM)(int*,float*,float*,int*,int*,int*,int*);

void c_aredam
#ifdef NeedFuncProto
(
    int *iam,
    float *xca,
    float *yca,
    int lca,
    int igi,
    int idl,
    int idr
)
#else
(iam,xca,yca,lca,igi,idl,idr)
    int *iam;
    float *xca;
    float *yca;
    int lca;
    int igi;
    int idl;
    int idr;
#endif
{
    NGCALLF(aredam,AREDAM)(iam,xca,yca,&lca,&igi,&idl,&idr);
}
