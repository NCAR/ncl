/*
 *      $Id: c_tdgtrs.c,v 1.2 1997-07-02 22:26:56 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdgtrs
#ifdef NeedFuncProto
(
    int    irst,
    int   *ifc1,
    int   *ifc2,
    int   *ifc3,
    int   *ifc4,
    int   *ilc1,
    int   *ilc2,
    int   *iltd,
    float *ustp,
    float *vstp,
    float *wstp
)
#else
(irst,ifc1,ifc2,ifc3,ifc4,ilc1,ilc2,iltd,ustp,vstp,wstp)
    int    irst;
    int   *ifc1;
    int   *ifc2;
    int   *ifc3;
    int   *ifc4;
    int   *ilc1;
    int   *ilc2;
    int   *iltd;
    float *ustp;
    float *vstp;
    float *wstp;
#endif
{
    int irst2;
    irst2=irst;
    NGCALLF(tdgtrs,TDGTRS)(&irst2,ifc1,ifc2,ifc3,ifc4,ilc1,
                                  ilc2,iltd,ustp,vstp,wstp);
}
