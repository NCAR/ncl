/*
 *	$Id: c_seter.c,v 1.1 1997-04-11 17:45:03 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_seter
#ifdef NeedFuncProto
(
    char *messg,
    int nerr,
    int iopt
)
#else
(messg,nerr,iopt)
    char *messg;
    int nerr;
    int iopt;
#endif
{
    NGstring messg2;
    int len;
    len = NGSTRLEN(messg);
    messg2 = NGCstrToFstr(messg,len);
    NGCALLF(seter,SETER)(messg2,&nerr,&iopt,len);
}
