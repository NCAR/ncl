/*
 *	$Id: c_dpsmth.c,v 1.1 1997-04-11 17:41:47 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpsmth
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
    NGCALLF(dpsmth,DPSMTH)(&xcpf2,&ycpf2,&ifvl);
}
