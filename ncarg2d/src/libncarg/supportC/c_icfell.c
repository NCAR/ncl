/*
 *	$Id: c_icfell.c,v 1.1 1997-04-11 17:44:55 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_icfell
#ifdef NeedFuncProto
(
    char *messg,
    int nerrf
)
#else
(messg,nerrf)
    char *messg;
    int nerrf;
#endif
{
    NGstring messg2;
    extern int NGCALLF(icfell,ICFELL)();
    int len;
    len = NGSTRLEN(messg);
    messg2 = NGCstrToFstr(messg,len);
    return(NGCALLF(icfell,ICFELL)(messg2,&nerrf,len));
}
