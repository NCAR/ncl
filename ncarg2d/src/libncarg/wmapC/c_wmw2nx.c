/*
 *	$Id: c_wmw2nx.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmw2nx,WMW2NX)(int*,float*,float*);

void c_wmw2nx
#ifdef NeedFuncProto
(
    int npt,
    float *p,
    float *q
)
#else
(npt,p,q)
    int npt;
    float *p;
    float *q;
#endif
{
    NGCALLF(wmw2nx,WMW2NX)(&npt,p,q);
}
