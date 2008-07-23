/*
 *	$Id: c_ppditr.c,v 1.5 2008-07-23 16:16:59 haley Exp $
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

extern void NGCALLF(ppditr,PPDITR)(float*,float*,int*,float*,float*,int*,
                                   float*,int*,int*,int (*urpt_)(),int*);

void c_ppditr
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
    NGCALLF(ppditr,PPDITR)(xccp,yccp,&nccp,xcsp,ycsp,&ncsp,rwrk,iwrk,&nwrk,
                           urpt_,ierr);
}
