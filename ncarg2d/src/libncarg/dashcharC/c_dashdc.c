/*
 *	$Id: c_dashdc.c,v 1.1 1997-04-11 17:41:34 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dashdc
#ifdef NeedFuncProto
(
    char *ipat,
    int jcrt,
    int jsize
)
#else
(ipat,jcrt,jsize)
    char *ipat;
    int jcrt;
    int jsize;
#endif
{
    NGstring ipat2;
    int len;

    len = NGSTRLEN(ipat);
    ipat2 = NGCstrToFstr(ipat,len);
    NGCALLF(dashdc,DASHDC)(ipat2,&jcrt,&jsize,len);
}
