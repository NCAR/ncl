/*
 *	$Id: c_pwrit.c,v 1.1 1997-04-11 17:44:28 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_pwrit
#ifdef NeedFuncProto
(
    float px,
    float py,
    char *ch,
    int nc,
    int is,
    int io,
    int ic
)
#else
(px,py,ch,nc,is,io,ic)
    float px;
    float py;
    char *ch;
    int nc;
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

    if( !ch ) nc = 0;

    len = NGSTRLEN(ch);
    ch2 = NGCstrToFstr(ch,len);
    NGCALLF(pwrit,PWRIT)(&px2,&py2,ch2,&nc,&is,&io,&ic,len);
}
