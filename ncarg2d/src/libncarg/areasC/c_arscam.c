/*
 *	$Id: c_arscam.c,v 1.1 1997-04-11 17:40:24 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_arscam
#ifdef NeedFuncProto
(
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*cpcolr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
               )
)
#else
(iam,xcs,ycs,mcs,iai,iag,mai,cpcolr_)
    int *iam;
    float *xcs;
    float *ycs;
    int mcs;
    int *iai;
    int *iag;
    int mai;
    int (*cpcolr_)();
#endif
{
    NGCALLF(arscam,ARSCAM)(iam,xcs,ycs,&mcs,iai,iag,&mai,cpcolr_);
}
