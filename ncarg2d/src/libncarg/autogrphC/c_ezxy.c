/*
 *	$Id: c_ezxy.c,v 1.1 1997-04-11 17:40:42 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezxy
#ifdef NeedFuncProto
(
    float *xdra,
    float *ydra,
    int npts,
    char *labg
)
#else
(xdra,ydra,npts,labg)
    float *xdra;
    float *ydra;
    int npts;
    char *labg;
#endif
{
	NGstring labg2;
    int len;

    len = NGSTRLEN(labg);
	labg2 = NGCstrToFstr(labg,len);
    NGCALLF(ezxy,EZXY)(xdra,ydra,&npts,labg2,len);
}
