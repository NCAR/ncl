/*
 *	$Id: c_strmln.c,v 1.1 1997-04-11 17:44:50 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_strmln
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *work,
    int imax,
    int iptsx,
    int jptsy,
    int nset,
    int *ier
)
#else
(u,v,work,imax,iptsx,jptsy,nset,ier)
    float *u;
    float *v;
    float *work;
    int imax;
    int iptsx;
    int jptsy;
    int nset;
    int *ier;
#endif
{
    NGCALLF(strmln,STRMLN)(u,v,work,&imax,&iptsx,&jptsy,&nset,ier);
}
