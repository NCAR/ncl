/*
 *      $Id: c_tdctri.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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

extern void NGCALLF(tdctri,TDCTRI)(float*,int*,int*,int*,float*);

void c_tdctri
#ifdef NeedFuncProto
(
    float *rtri,
    int    mtri,
    int   *ntri,
    int    iaxs,
    float  rcut
)
#else
(rtri,mtri,ntri,iaxs,rcut)
    float *rtri;
    int    mtri;
    int   *ntri;
    int    iaxs;
    float  rcut;
#endif
{
    NGCALLF(tdctri,TDCTRI)(rtri,&mtri,ntri,&iaxs,&rcut);
}
