/*
 *	$Id: c_ppuntr.c,v 1.1 1997-04-11 17:44:00 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ppuntr
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
    int (*urpt_)(
        float *xcbl,
        float *xcbr,
        float *ycob,
        float *dxle,
        float *dxre,
		float *ycot
               ),
	int *ierr
)
#else
(xccp,yccp,nccp,xcsp,ycsp,ncsp,rwrk,iwrk,nwrk,urpt_,ierr)
	float *xccp;
	float *yccp;
	int nccp;
	float *xcsp;
	float *ycsp;
	int ncsp;
	float *rwrk;
	int *iwrk;
	int nwrk;
	int (*urpt_)();
	int *ierr;
#endif
{
    NGCALLF(ppuntr,PPUNTR)(xccp,yccp,&nccp,xcsp,ycsp,&ncsp,rwrk,iwrk,&nwrk,urpt_,ierr);
}
