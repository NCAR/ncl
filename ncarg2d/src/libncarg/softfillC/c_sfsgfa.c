/*
 *	$Id: c_sfsgfa.c,v 1.1 1997-04-11 17:44:11 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfsgfa
#ifdef NeedFuncProto
(
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd,
    int ici
)
#else
(xra,yra,nra,dst,nst,ind,nnd,ici)
    float *xra;
    float *yra;
    int nra;
    float *dst;
    int nst;
    int *ind;
    int nnd;
    int ici;
#endif
{
    NGCALLF(sfsgfa,SFSGFA)(xra,yra,&nra,dst,&nst,ind,&nnd,&ici);
}
