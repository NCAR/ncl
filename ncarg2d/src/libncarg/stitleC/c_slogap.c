/*
 *      $Id: c_slogap.c,v 1.5 2008-07-23 16:17:03 haley Exp $
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

extern void NGCALLF(slogap,SLOGAP)(float*,int*);

void c_slogap
#ifdef NeedFuncProto
(
    float time,
    int mtst
)
#else
(time,mtst)
    float time;
    int mtst;
#endif
{
    NGCALLF(slogap,SLOGAP)(&time,&mtst);
}
