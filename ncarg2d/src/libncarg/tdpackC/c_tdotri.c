/*
 *      $Id: c_tdotri.c,v 1.1 1997-06-30 21:47:40 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdotri
#ifdef NeedFuncProto
(
    float *rtri,
    int    mtri,
    int   *ntri,
    float *rtwk,
    int   *itwk,
    int    iord
)
#else
(rtri,mtri,ntri,rtwk,itwk,iord)
    float *rtri;
    int    mtri;
    int   *ntri;
    float *rtwk;
    int   *itwk;
    int    iord;
#endif
{
    int mtri2,iord2;
    mtri2=mtri;
    iord2=iord;
    NGCALLF(tdotri,TDOTRI)(rtri,&mtri2,ntri,rtwk,itwk,&iord2);
}
