/*
 *      $Id: c_tddtri.c,v 1.2 1997-07-02 22:26:54 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tddtri
#ifdef NeedFuncProto
(
    float *rtri,
    int    mtri,
    int   *ntri,
    int   *itwk
)
#else
(rtri,mtri,ntri,itwk)
    float *rtri;
    int    mtri;
    int   *ntri;
    int   *itwk;
#endif
{
    int mtri2;
    mtri2=mtri;
    NGCALLF(tddtri,TDDTRI)(rtri,&mtri2,ntri,itwk);
}
