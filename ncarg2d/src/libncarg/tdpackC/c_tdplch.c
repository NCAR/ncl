/*
 *      $Id: c_tdplch.c,v 1.5 2008-07-23 16:17:06 haley Exp $
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

extern void NGCALLF(tdplch,TDPLCH)(float*,float*,NGstring,float*,float*,
                                   float*,int);

void c_tdplch
#ifdef NeedFuncProto
(
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
)
#else
(xpos,ypos,chrs,size,angd,cntr)
    float xpos;
    float ypos;
    char *chrs;
    float size;
    float angd;
    float cntr;
#endif
{
    int lochrs;
    NGstring chrs2;
    lochrs=NGSTRLEN(chrs);
    chrs2=NGCstrToFstr(chrs,lochrs);
    NGCALLF(tdplch,TDPLCH)(&xpos,&ypos,chrs2,&size,&angd,&cntr,lochrs);
}
