/*
 *	$Id: c_sfnorm.c,v 1.1 1997-04-11 17:44:09 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfnorm
#ifdef NeedFuncProto
(
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd
)
#else
(xra,yra,nra,dst,nst,ind,nnd)
    float *xra;
    float *yra;
    int nra;
    float *dst;
    int nst;
    int *ind;
    int nnd;
#endif
{
    NGCALLF(sfnorm,SFNORM)(xra,yra,&nra,dst,&nst,ind,&nnd);
}    
