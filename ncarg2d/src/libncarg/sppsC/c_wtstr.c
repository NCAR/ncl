/*
 *	$Id: c_wtstr.c,v 1.1 1997-04-11 17:44:30 haley Exp $
 */
#include <ncarg/ncargC.h>

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
    float px2,py2;
    NGstring ch2;
    int len;
    px2 = px;
    py2 = py;
    len = NGSTRLEN(ch);
    ch2 = NGCstrToFstr(ch,len);
    NGCALLF(wtstr,WTSTR)(&px2,&py2,ch2,&is,&io,&ic,len);
}
