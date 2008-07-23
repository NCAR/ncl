/*
 *      $Id: c_tddtri.c,v 1.6 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(tddtri,TDDTRI)(float*,int*,int*,int*);

void c_tddtri
#ifdef NeedFuncProto
(
    float *rtri,
    int    mtri,
    int   *ntri,
    int   *itwk
)
#else
(rtri,mtri,ntri,itwk)
    float *rtri;
    int    mtri;
    int   *ntri;
    int   *itwk;
#endif
{
    NGCALLF(tddtri,TDDTRI)(rtri,&mtri,ntri,itwk);
}
