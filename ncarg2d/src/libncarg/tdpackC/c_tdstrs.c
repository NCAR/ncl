/*
 *      $Id: c_tdstrs.c,v 1.2 1997-07-02 22:27:00 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdstrs
#ifdef NeedFuncProto
(
    int   irst,
    int   ifc1,
    int   ifc2,
    int   ifc3,
    int   ifc4,
    int   ilc1,
    int   ilc2,
    int   iltd,
    float ustp,
    float vstp,
    float wstp
)
#else
(irst,ifc1,ifc2,ifc3,ifc4,ilc1,ilc2,iltd,ustp,vstp,wstp)
    int   irst;
    int   ifc1;
    int   ifc2;
    int   ifc3;
    int   ifc4;
    int   ilc1;
    int   ilc2;
    int   iltd;
    float ustp;
    float vstp;
    float wstp;
#endif
{
    int irst2,ifc12,ifc22,ifc32,ifc42,ilc12,ilc22,iltd2;
    float ustp2,vstp2,wstp2;
    irst2=irst;
    ifc12=ifc1;
    ifc22=ifc2;
    ifc32=ifc3;
    ifc42=ifc4;
    ilc12=ilc1;
    ilc22=ilc2;
    iltd2=iltd;
    ustp2=ustp;
    vstp2=vstp;
    wstp2=wstp;
    NGCALLF(tdstrs,TDSTRS)(&irst2,&ifc12,&ifc22,&ifc32,&ifc42,&ilc12,&ilc22,
                                                &iltd2,&ustp2,&vstp2,&wstp2);
}
