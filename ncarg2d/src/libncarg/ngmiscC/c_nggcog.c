/*
 *	$Id: c_nggcog.c,v 1.1 1997-04-11 17:43:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_nggcog
#ifdef NeedFuncProto
(
    float clat,
    float clon,
    float crad,
    float *alat,
    float *alon,
    int npts
)
#else
(clat,clon,crad,alat,alon,npts)
    float clat;
    float clon;
    float crad;
    float *alat;
    float *alon;
    int npts;
#endif
{
    float clat2, clon2, crad2;
	clat2 = clat;
	clon2 = clon;
	crad2 = crad;
    NGCALLF(nggcog,NGGCOG)(&clat2,&clon2,&crad2,alat,alon,&npts);
}
