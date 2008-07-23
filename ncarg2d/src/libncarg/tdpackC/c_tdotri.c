/*
 *      $Id: c_tdotri.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdotri,TDOTRI)(float*,int*,int*,float*,int*,int*);

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
    NGCALLF(tdotri,TDOTRI)(rtri,&mtri,ntri,rtwk,itwk,&iord);
}
