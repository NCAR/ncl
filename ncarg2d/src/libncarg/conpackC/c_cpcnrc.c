/*
 *	$Id: c_cpcnrc.c,v 1.1 1997-04-11 17:41:02 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpcnrc
#ifdef NeedFuncProto
(
    float *zdat,
    int kzdt,
    int mzdt,
    int nzdt,
    float flow,
    float fhgh,
    float finc,
    int kset,
    int nhgh,
    int ndsh
)
#else
(zdat,kzdt,mzdt,nzdt,flow,fhgh,finc,kset,nhgh,ndsh)
    float *zdat;
    int kzdt;
    int mzdt;
    int nzdt;
    float flow;
    float fhgh;
    float finc;
    int kset;
    int nhgh;
    int ndsh;
#endif
{
    float y1,y2,y3;

    y1 = flow;
    y2 = fhgh;
    y3 = finc;

    NGCALLF(cpcnrc,CPCNRC)(zdat,&kzdt,&mzdt,&nzdt,&y1,&y2,&y3,&kset,&nhgh,&ndsh);
}
