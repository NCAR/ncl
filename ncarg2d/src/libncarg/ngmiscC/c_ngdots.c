/*
 *	$Id: c_ngdots.c,v 1.5 2008-07-23 16:16:57 haley Exp $
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

extern void NGCALLF(ngdots,NGDOTS)(float*,float*,int*,float*,int*);

void c_ngdots
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    int num,
    float size,
    int icolor
)
#else
(x,y,num,size,icolor)
    float *x;
    float *y;
    int num;
    float size;
    int icolor;
#endif
{
    NGCALLF(ngdots,NGDOTS)(x,y,&num,&size,&icolor);
}
