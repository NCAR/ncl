/*
 *      $Id: c_tdsort.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(tdsort,TDSORT)(float*,int*,int*,int*);

void c_tdsort
#ifdef NeedFuncProto
(
    float *rwrk,
    int    nwrk,
    int    iord,
    int   *iwrk
)
#else
(rwrk,nrwk,iord,iwrk)
    float *rwrk;
    int    nwrk;
    int    iord;
    int   *iwrk;
#endif
{
    NGCALLF(tdsort,TDSORT)(rwrk,&nwrk,&iord,iwrk);
}
