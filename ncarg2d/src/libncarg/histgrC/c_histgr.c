/*
 *	$Id: c_histgr.c,v 1.1 1997-04-11 17:43:15 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_histgr
#ifdef NeedFuncProto
(
    float *dat1,
    int ndim,
    int npts,
    int iflag,
    float *class_values,
    int nclass,
    float *wrk,
    int nwrk
)
#else
(dat1,ndim,npts,iflag,class_values,nclass,wrk,nwrk)
    float *dat1;
    int ndim;
    int npts;
    int iflag;
    float *class_values;
    int nclass;
    float *wrk;
    int nwrk;
#endif
{
    NGCALLF(histgr,HISTGR)(dat1,&ndim,&npts,&iflag,class_values,&nclass,wrk,&nwrk);
}
