/*
 *	$Id: c_halfax.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(halfax,HALFAX)(int*,int*,int*,int*,float*,float*,int*,
                                   int*);

void c_halfax
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    float xint,
    float yint,
    int ixlb,
    int iylb
)
#else
(mjrx,mnrx,mjry,mnry,xint,yint,ixlb,iylb)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
    float xint;
    float yint;
    int ixlb;
    int iylb;
#endif
{
    NGCALLF(halfax,HALFAX)(&mjrx,&mnrx,&mjry,&mnry,&xint,&yint,&ixlb,&iylb);
}
