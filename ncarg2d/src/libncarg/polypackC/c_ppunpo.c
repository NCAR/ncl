/*
 *	$Id: c_ppunpo.c,v 1.5 2008-07-23 16:16:59 haley Exp $
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

extern void NGCALLF(ppunpo,PPUNPO)(float*,float*,int*,float*,float*,int*,
                                   float*,int*,int*,int (*urpp_)(),int*);

void c_ppunpo
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
    NGCALLF(ppunpo,PPUNPO)(xccp,yccp,&nccp,xcsp,ycsp,&ncsp,rwrk,iwrk,&nwrk,
                           urpp_,ierr);
}
