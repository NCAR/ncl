/*
 *	$Id: c_stream.c,v 1.1 1997-04-11 17:44:49 haley Exp $
 */
#include <ncarg/ncargC.h>

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
