/*
 *	$Id: c_plchlq.c,v 1.1 1997-04-11 17:43:54 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_plchlq
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
    float xpos2,ypos2,size2,angd2,cntr2;
    NGstring chrs2;
    int len;
    xpos2 = xpos;
    ypos2 = ypos;
    size2 = size;
    angd2 = angd;
    cntr2 = cntr;
    len = NGSTRLEN(chrs);
    chrs2 = NGCstrToFstr(chrs,len);
    NGCALLF(plchlq,PLCHLQ)(&xpos2,&ypos2,chrs2,&size2,&angd2,&cntr2,len);
}
