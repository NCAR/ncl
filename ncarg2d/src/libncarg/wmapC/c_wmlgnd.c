/*
 *	$Id: c_wmlgnd.c,v 1.5 2008-07-23 16:17:09 haley Exp $
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

extern void NGCALLF(wmlgnd,WMLGND)(float*,float*,int*,int*,int*);

void c_wmlgnd
#ifdef NeedFuncProto
(
    float x,
    float y,
    int ntype,
    int irows,
    int icols
)
#else
(x,y,ntype,irows,icols)
    float x;
    float y;
    int ntype;
    int irows;
    int icols;
#endif
{
    NGCALLF(wmlgnd,WMLGND)(&x,&y,&ntype,&irows,&icols);
}
