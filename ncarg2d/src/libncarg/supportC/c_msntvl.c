/*
 *	$Id: c_msntvl.c,v 1.1 1997-04-11 17:44:58 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_msntvl
#ifdef NeedFuncProto
(
    int n,
    float t, 
    float *x
)
#else
(n,t,x)
    int n;
    float t; 
    float *x;
#endif
{
    float t2;
    extern int NGCALLF(msntvl,MSNTVL)();
    t2 = t;
    return(NGCALLF(msntvl,MSNTVL)(&t2,x,&n));
}
