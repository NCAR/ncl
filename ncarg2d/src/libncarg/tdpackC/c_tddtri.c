/*
 *      $Id: c_tddtri.c,v 1.1 1997-06-30 21:47:24 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tddtri
#ifdef NeedFuncProto
(
    float *rtri,
    int    mtri,
    int   *ntri,
    int   *itri
)
#else
(rtri,mtri,ntri,itri)
    float *rtri;
    int    mtri;
    int   *ntri;
    int   *itri;
#endif
{
    int mtri2;
    mtri2=mtri;
    NGCALLF(tddtri,TDDTRI)(rtri,&mtri2,ntri,itri);
}
