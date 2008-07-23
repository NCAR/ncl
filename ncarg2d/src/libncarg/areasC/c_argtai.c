/*
 *	$Id: c_argtai.c,v 1.5 2008-07-23 16:16:39 haley Exp $
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

extern void NGCALLF(argtai,ARGTAI)(int*,float*,float*,int*,int*,int*,
                                   int*,int*);

void c_argtai
#ifdef NeedFuncProto
(
    int *iam,
    float xcd,
    float ycd,
    int *iai,
    int *iag,
    int mai,
    int *nai,
    int icf
)
#else
(iam,xcd,ycd,iai,iag,mai,nai,icf)
    int *iam;
    float xcd;
    float ycd; 
    int *iai;
    int *iag;
    int mai;
    int *nai;
    int icf;
#endif
{
    NGCALLF(argtai,ARGTAI)(iam,&xcd,&ycd,iai,iag,&mai,nai,&icf);
}
