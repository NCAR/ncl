/*
 *	$Id: c_wmdrrg.c,v 1.1 1997-04-11 17:45:24 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmdrrg
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    char *itype,
    int nc,
    float *xc,
    float *yc
)
#else
(n,x,y,itype,nc,xc,yc)
    int n;
    float *x;
    float *y;
    char *itype;
    int nc;
    float *xc;
    float *yc
#endif
{
	NGstring itype2;
	int len;
	len = NGSTRLEN(itype);
	itype2 = NGCstrToFstr(itype,len);
    NGCALLF(wmdrrg,WMDRRG)(&n,x,y,itype2,&nc,xc,yc,len);
}
