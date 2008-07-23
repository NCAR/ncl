/*
 *	$Id: c_line.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(line,LINE)(float*,float*,float*,float*);

void c_line
#ifdef NeedFuncProto
(
    float x1,
    float y1,
    float x2,
    float y2
)
#else
(x1,y1,x2,y2)
    float x1;
    float y1;
    float x2;
    float y2;
#endif
{
    NGCALLF(line,LINE)(&x1,&y1,&x2,&y2);
}
