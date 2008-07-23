/*
 *	$Id: c_wtstr.c,v 1.5 2008-07-23 16:17:02 haley Exp $
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

extern void NGCALLF(wtstr,WTSTR)(float*,float*,NGstring,int*,int*,int*,int);

void c_wtstr
#ifdef NeedFuncProto
(
    float px,
    float py,
    char *ch,
    int is,
    int io,
    int ic
)
#else
(px,py,ch,is,io,ic)
    float px;
    float py;
    char *ch;
    int is;
    int io;
    int ic;
#endif
{
    NGstring ch2;
    int len;
    len = NGSTRLEN(ch);
    ch2 = NGCstrToFstr(ch,len);
    NGCALLF(wtstr,WTSTR)(&px,&py,ch2,&is,&io,&ic,len);
}
