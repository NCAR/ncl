/*
 *	$Id: c_agstup.c,v 1.5 2008-07-23 16:16:41 haley Exp $
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

extern void NGCALLF(agstup,AGSTUP)(float*,int*,int*,int*,int*,float*,int*,
                                   int*,int*,int*);

void c_agstup
#ifdef NeedFuncProto
(
    float *xdra,
    int nvix,
    int iivx,
    int nevx,
    int iiex,
    float *ydra,
    int nviy,
    int iivy,
    int nevy,
    int iiey
)
#else
(xdra,nvix,iivx,nevx,iiex,ydra,nviy,iivy,nevy,iiey)
    float *xdra;
    int nvix;
    int iivx;
    int nevx;
    int iiex;
    float *ydra;
    int nviy;
    int iivy;
    int nevy;
    int iiey;
#endif
{
    NGCALLF(agstup,AGSTUP)(xdra,&nvix,&iivx,&nevx,&iiex,ydra,&nviy,&iivy,&nevy,&iiey);
}
