/*
 *      $Id: c_tdctri.c,v 1.1 2000-02-17 00:22:35 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdctri
#ifdef NeedFuncProto
(
    float *rtri,
    int    mtri,
    int   *ntri,
    int    iaxs,
    float  rcut
)
#else
(rtri,mtri,ntri,iaxs,rcut)
    float *rtri;
    int    mtri;
    int   *ntri;
    int    iaxs;
    float  rcut;
#endif
{
    int mtri2;
    mtri2=mtri;
    NGCALLF(tdctri,TDCTRI)(rtri,&mtri2,ntri,iaxs,rcut);
}
