/*
 *      $Id: c_tdplch.c,v 1.1 1997-06-30 21:47:42 kennison Exp $
 */
#include <ncarg/ncargC.h>

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
    float xpos2,ypos2;
    int lochrs;
    NGstring chrs2;
    float size2,angd2,cntr2;
    xpos2=xpos;
    ypos2=ypos;
    lochrs=NGSTRLEN(chrs);
    chrs2=NGCstrToFstr(chrs,lochrs);
    size2=size;
    angd2=angd;
    cntr2=cntr;
    NGCALLF(tdplch,TDPLCH)(&xpos2,&ypos2,chrs2,&size2,&angd2,&cntr2,lochrs);
}
