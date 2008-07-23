/*
 *	$Id: c_perim3.c,v 1.5 2008-07-23 16:17:07 haley Exp $
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

extern void NGCALLF(perim3,PERIM3)(int*,int*,int*,int*,int*,float*);

void c_perim3 
#ifdef NeedFuncProto
(
    int magr1,
    int mini1,
    int magr2,
    int mini2,
    int iwhich,
    float var
)
#else
 (magr1,mini1,magr2,mini2,iwhich,var)
    int magr1;
    int mini1;
    int magr2;
    int mini2;
    int iwhich;
    float var;
#endif
{
    NGCALLF(perim3,PERIM3)(&magr1,&mini1,&magr2,&mini2,&iwhich,&var);
}
