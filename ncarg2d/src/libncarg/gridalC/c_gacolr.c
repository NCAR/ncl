/*
 *	$Id: c_gacolr.c,v 1.1 1997-04-11 17:42:39 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gacolr
#ifdef NeedFuncProto
(
    int kaxs,
    int klbl,
    int kmjt,
    int kmnt
)
#else
(kaxs,klbl,kmjt,kmnt)
    int kaxs;
    int klbl;
    int kmjt;
    int kmnt;
#endif
{
    NGCALLF(gacolr,GACOLR)(&kaxs,&klbl,&kmjt,&kmnt);
}
