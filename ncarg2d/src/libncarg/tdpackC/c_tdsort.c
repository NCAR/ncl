/*
 *      $Id: c_tdsort.c,v 1.1 1997-06-30 21:47:50 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdsort
#ifdef NeedFuncProto
(
    float *rwrk,
    int    nwrk,
    int    iord,
    int   *iwrk
)
#else
(rwrk,nrwk,iord,iwrk)
    float *rwrk;
    int    nwrk;
    int    iord;
    int   *iwrk;
#endif
{
    int nwrk2,iord2;
    nwrk2=nwrk;
    iord2=iord;
    NGCALLF(tdsort,TDSORT)(rwrk,&nwrk2,&iord2,iwrk);
}
