/*
 *	$Id: c_cpmvrw.c,v 1.1 1997-04-11 17:41:08 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpmvrw
#ifdef NeedFuncProto
(
    float *rwko,
    float *rwrk,
    int lwkn
)
#else
(rwko,rwrk,lwkn)
    float *rwko;
    float *rwrk;
    int lwkn;
#endif
{
    NGCALLF(cpmvrw,CPMVRW)(rwko,rwrk,&lwkn);
}
