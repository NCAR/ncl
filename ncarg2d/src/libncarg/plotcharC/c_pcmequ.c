/*
 *	$Id: c_pcmequ.c,v 1.5 2008-07-23 15:46:49 haley Exp $
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

extern void NGCALLF(pcmequ,PCMEQU)(float*,float*,NGstring,float*,float*,
                                   float*,int);

void c_pcmequ
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
    NGstring chrs2;
    int len;
    len = NGSTRLEN(chrs);
    chrs2 = NGCstrToFstr(chrs,len);
    NGCALLF(pcmequ,PCMEQU)(&xpos,&ypos,chrs2,&size,&angd,&cntr,len);
}
