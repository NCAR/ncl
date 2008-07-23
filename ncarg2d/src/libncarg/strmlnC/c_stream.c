/*
 *	$Id: c_stream.c,v 1.5 2008-07-23 16:17:04 haley Exp $
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

extern void NGCALLF(stream,STREAM)(float*,float*,float*,int*,
                                   int (*stumsl_)(),float*);

void c_stream
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *p,
    int *iam,
    int (*stumsl_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
    ),
    float *wrk
)
#else
(u,v,p,iam,stumsl_,wrk)
    float *u;
    float *v;
    float *p;
    int *iam;
    int (*stumsl_)();
    float *wrk;
#endif
{
    NGCALLF(stream,STREAM)(u,v,p,iam,stumsl_,wrk);
}
