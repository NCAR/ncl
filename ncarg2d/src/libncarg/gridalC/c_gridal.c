/*
 *	$Id: c_gridal.c,v 1.5 2008-07-23 16:16:55 haley Exp $
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

extern void NGCALLF(gridal,GRIDAL)(int*,int*,int*,int*,int*,int*,int*,
                                   float*,float*);

void c_gridal
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    int ixlb,
    int iylb,
    int igph,
    float xint,
    float yint
)
#else
(mjrx,mnrx,mjry,mnry,ixlb,iylb,igph,xint,yint)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
    int ixlb;
    int iylb;
    int igph;
    float xint;
    float yint;
#endif
{
    NGCALLF(gridal,GRIDAL)(&mjrx,&mnrx,&mjry,&mnry,&ixlb,&iylb,&igph,
                           &xint,&yint);
}
