/*
 *  $Id: c_setr.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(setr,SETR)(float*,float*,float*,float*,float*,float*,
                               float*);

void c_setr
#ifdef NeedFuncProto
(
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float zmin,
    float zmax,
    float r0
)
#else
(xmin,xmax,ymin,ymax,zmin,zmax,r0)
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float zmin;
    float zmax;
    float r0;
#endif
{
    NGCALLF(setr,SETR)(&xmin,&xmax,&ymin,&ymax,&zmin,&zmax,&r0);
}
