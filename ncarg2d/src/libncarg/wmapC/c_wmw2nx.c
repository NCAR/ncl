/*
 *	$Id: c_wmw2nx.c,v 1.1 1997-04-11 17:45:32 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmw2nx
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
    NGCALLF(wmw2nx,WMW2NX)(&npt,p,q);
}
