/*
 *      $Id: c_tdstrs.c,v 1.1 1997-06-30 21:47:54 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdstrs
#ifdef NeedFuncProto
(
    int   irst,
    int   ia01,
    int   ia02,
    int   ia03,
    int   ia04,
    int   ia05,
    int   ia06,
    int   ia07,
    float ra08,
    float ra09,
    float ra10
)
#else
(irst,ia01,ia02,ia03,ia04,ia05,ia06,ia07,ra08,ra09,ra10)
    int   irst;
    int   ia01;
    int   ia02;
    int   ia03;
    int   ia04;
    int   ia05;
    int   ia06;
    int   ia07;
    float ra08;
    float ra09;
    float ra10;
#endif
{
    int irst2,ia012,ia022,ia032,ia042,ia052,ia062,ia072;
    float ra082,ra092,ra102;
    irst2=irst;
    ia012=ia01;
    ia022=ia02;
    ia032=ia03;
    ia042=ia04;
    ia052=ia05;
    ia062=ia06;
    ia072=ia07;
    ra082=ra08;
    ra092=ra09;
    ra102=ra10;
    NGCALLF(tdstrs,TDSTRS)(&irst2,&ia012,&ia022,&ia032,&ia042,&ia052,
                                  &ia062,&ia072,&ra082,&ra092,&ra102);
}
