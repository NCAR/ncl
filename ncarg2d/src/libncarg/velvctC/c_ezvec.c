/*
 *	$Id: c_ezvec.c,v 1.1 1997-04-11 17:45:15 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ezvec
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    int m,
    int n
)
#else
(u,v,m,n)
    float *u;
    float *v;
    int m;
    int n;
#endif
{
    NGCALLF(ezvec,EZVEC)(u,v,&m,&n);
}
