/*
 *	$Id: c_ppppap.c,v 1.5 2008-07-23 16:16:59 haley Exp $
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

extern void NGCALLF(ppppap,PPPPAP)(float*,float*,int*,int*);

void c_ppppap
#ifdef NeedFuncProto
(
        float *xcop,
        float *ycop,
        int ncop,
        int nbts
)
#else
(xcop,ycop,ncop,nbts)
        float *xcop;
        float *ycop;
        int ncop;
        int nbts;
)
#endif
{
    NGCALLF(ppppap,PPPPAP)(xcop,ycop,&ncop,&nbts);
}
