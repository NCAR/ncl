/*
 *	$Id: c_dpdraw.c,v 1.1 1997-04-11 17:41:41 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpdraw
#ifdef NeedFuncProto
(
    float xcpf,
    float ycpf,
    int ifvl
)
#else
(xcpf,ycpf,ifvl)
    float xcpf;
    float ycpf;
    int ifvl;
#endif
{
    float xcpf2, ycpf2;

    xcpf2 = xcpf;
    ycpf2 = ycpf;
    NGCALLF(dpdraw,DPDRAW)(&xcpf2,&ycpf2,&ifvl);
}
