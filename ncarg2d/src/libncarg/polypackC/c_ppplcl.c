/*
 *	$Id: c_ppplcl.c,v 1.5 2008-07-23 16:16:59 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(ppplcl,PPPLCL)(float*,float*,float*,float*,float*,
                                   float*,int*,float*,int*,int (*urpf_)(),
                                   int*);

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
    NGCALLF(ppplcl,PPPLCL)(&xmin,&xmax,&ymin,&ymax,xcpl,ycpl,&ncpl,
                           rwrk,&lwrk,urpf_,ierr);
}
