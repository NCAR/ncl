/*
 *	$Id: c_wmw2ny.c,v 1.1 1997-04-11 17:45:33 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmw2ny
#ifdef NeedFuncProto
(
    int npt,
    float *p,
    float *q
)
#else
(npt,p,q)
    int npt;
    float *p;
    float *q;
#endif
{
    NGCALLF(wmw2ny,WMW2NY)(&npt,p,q);
}
