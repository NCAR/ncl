/*
 *	$Id: c_ftitle.c,v 1.1 1997-04-11 17:44:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ftitle
#ifdef NeedFuncProto
(
    int mtst
)
#else
(mtst)
    int mtst;
#endif
{
    NGCALLF(ftitle,FTITLE)(&mtst);
}
