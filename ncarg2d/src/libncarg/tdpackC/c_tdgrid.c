/*
 *      $Id: c_tdgrid.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdgrid,TDGRID)(float*,float*,int*,float*,float*,int*,
                                   int*);

void c_tdgrid
#ifdef NeedFuncProto
(
    float xbeg,
    float xstp,
    int   noxs,
    float ybeg,
    float ystp,
    int   noys,
    int   igrd
)
#else
(xbeg,xstp,noxs,ybeg,ystp,noys,igrd)
    float xbeg;
    float xstp;
    int   noxs;
    float ybeg;
    float ystp;
    int   noys;
    int   igrd;
#endif
{
    NGCALLF(tdgrid,TDGRID)(&xbeg,&xstp,&noxs,&ybeg,&ystp,&noys,&igrd);
}
