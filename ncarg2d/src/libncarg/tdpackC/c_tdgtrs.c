/*
 *      $Id: c_tdgtrs.c,v 1.1 1997-06-30 21:47:30 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdgtrs
#ifdef NeedFuncProto
(
    int    irst,
    int   *ia01,
    int   *ia02,
    int   *ia03,
    int   *ia04,
    int   *ia05,
    int   *ia06,
    int   *ia07,
    float *ra08,
    float *ra09,
    float *ra10
)
#else
(irst,ia01,ia02,ia03,ia04,ia05,ia06,ia07,ra08,ra09,ra10)
    int    irst;
    int   *ia01;
    int   *ia02;
    int   *ia03;
    int   *ia04;
    int   *ia05;
    int   *ia06;
    int   *ia07;
    float *ra08;
    float *ra09;
    float *ra10;
#endif
{
    int irst2;
    irst2=irst;
    NGCALLF(tdgtrs,TDGTRS)(&irst2,ia01,ia02,ia03,ia04,ia05,
                                  ia06,ia07,ra08,ra09,ra10);
}
