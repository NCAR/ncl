/*
 *	$Id: c_plchhq.c,v 1.1 1997-04-11 17:43:54 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_plchhq
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
    float y1,y2,y3,y4,y5;
    NGstring chrs2;
    int len;
    y1 = xpos;
    y2 = ypos;
    y3 = size;
    y4 = angd;
    y5 = cntr;
    len = NGSTRLEN(chrs);
    chrs2 = NGCstrToFstr(chrs,len);
    NGCALLF(plchhq,PLCHHQ)(&y1,&y2,chrs2,&y3,&y4,&y5,len);
}
