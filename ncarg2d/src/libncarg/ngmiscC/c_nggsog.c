/*
 *	$Id: c_nggsog.c,v 1.1 1997-04-11 17:43:39 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_nggsog
#ifdef NeedFuncProto
(
    float slat,
    float slon,
    float srad,
    float *alat,
    float *alon
)
#else
(slat,slon,srad,alat,alon)
    float slat;
    float slon;
    float srad;
    float *alat;
    float *alon;
#endif
{
    float slat2, slon2, srad2;
	slat2 = slat;
	slon2 = slon;
	srad2 = srad;
    NGCALLF(nggsog,NGGSOG)(&slat2,&slon2,&srad2,alat,alon);
}
