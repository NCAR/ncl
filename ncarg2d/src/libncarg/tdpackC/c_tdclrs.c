/*
 *      $Id: c_tdclrs.c,v 1.1 1997-06-30 21:47:22 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdclrs
#ifdef NeedFuncProto
(
    int   iwid,
    int   ibow,
    float shde,
    float shdr,
    int   iofc,
    int   iolc,
    int   ilmt
)
#else
(iwid,ibow,shde,shdr,iofc,iolc,ilmt)
    int   iwid;
    int   ibow;
    float shde;
    float shdr;
    int   iofc;
    int   iolc;
    int   ilmt;
#endif
{
    int iwid2,ibow2;
    float shde2,shdr2;
    int iofc2,iolc2,ilmt2;
    iwid2=iwid;
    ibow2=ibow;
    shde2=shde;
    shdr2=shdr;
    iofc2=iofc;
    iolc2=iolc;
    ilmt2=ilmt;
    NGCALLF(tdclrs,TDCLRS)(&iwid2,&ibow2,&shde2,&shdr2,&iofc2,&iolc2,&ilmt2);
}
