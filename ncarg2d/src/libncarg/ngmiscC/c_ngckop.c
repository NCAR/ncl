/*
 *	$Id: c_ngckop.c,v 1.1 1997-04-11 17:43:36 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_ngckop
#ifdef NeedFuncProto
(
    int wkid
)
#else
(wkid)
    int wkid;
#endif
{
    extern int NGCALLF(ngckop,NGCKOP)();
    return(NGCALLF(ngckop,NGCKOP)(&wkid));
}
