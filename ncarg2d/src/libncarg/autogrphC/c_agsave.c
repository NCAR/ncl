/*
 *	$Id: c_agsave.c,v 1.1 1997-04-11 17:40:36 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agsave
#ifdef NeedFuncProto
(
    int ifno
)
#else
(ifno)
    int ifno;
#endif
{
    NGCALLF(agsave,AGSAVE)(&ifno);
}
