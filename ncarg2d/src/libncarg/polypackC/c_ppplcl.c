/*
 *	$Id: c_ppplcl.c,v 1.1 1997-04-11 17:43:59 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ppplcl
#ifdef NeedFuncProto
(
	float xmin,
	float xmax,
	float ymin,
	float ymax,
	float *xcpl,
	float *ycpl,
	int ncpl,
	float *rwrk,
	int lwrk,
    int (*urpf_)(
        float *xcra,
        float *ycra,
 	 	int *ncra
               ),
	int *ierr
)
#else
(xmin,xmax,ymin,ymax,xcpl,ycpl,ncpl,rwrk,lwrk,urpf_,ierr)
	float xmin;
	float xmax;
	float ymin;
	float ymax;
	float *xcpl;
	float *ycpl;
	int ncpl;
	float *rwrk;
	int lwrk;
    int (*urpf_)();
	int *ierr;
#endif
{
	float xmin2,xmax2,ymin2,ymax2;
	xmin2 = xmin;
	xmax2 = xmax;
	ymin2 = ymin;
	ymax2 = ymax;
    NGCALLF(ppplcl,PPPLCL)(&xmin2,&xmax2,&ymin2,&ymax2,xcpl,ycpl,&ncpl,rwrk,&lwrk,urpf_,ierr);
}
