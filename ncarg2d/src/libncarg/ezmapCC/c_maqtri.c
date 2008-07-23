/*
 *	$Id: c_maqtri.c,v 1.2 2008-07-23 16:16:48 haley Exp $
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

extern void NGCALLF(maqtri,MAQTRI)(float*,float*,float*,float*);

void c_maqtri
#ifdef NeedFuncProto
(
    float uval,
    float vval,
    float *rlat,
    float *rlon
)
#else
(uval,vval,rlat,rlon)
    float uval;
    float vval;
    float *rlat;
    float *rlon;
#endif
{
    NGCALLF(maqtri,MAQTRI)(&uval,&vval,rlat,rlon);
}
