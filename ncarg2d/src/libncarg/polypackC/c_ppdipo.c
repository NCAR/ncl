/*
 *	$Id: c_ppdipo.c,v 1.1 1997-04-11 17:43:57 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ppdipo
#ifdef NeedFuncProto
(
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpp_)(
        float *xcra,
        float *ycra,
        int *ncra
               ),
        int *ierr
)
#else
(xccp,yccp,nccp,xcsp,ycsp,ncsp,rwrk,iwrk,nwrk,urpp_,ierr)
        float *xccp;
        float *yccp;
        int nccp;
        float *xcsp;
        float *ycsp;
        int ncsp;
        float *rwrk;
        int *iwrk;
        int nwrk;
        int (*urpp_)();
        int *ierr;
#endif
{
    NGCALLF(ppdipo,PPDIPO)(xccp,yccp,&nccp,xcsp,ycsp,&ncsp,rwrk,iwrk,&nwrk,urpp_,ierr);
}
