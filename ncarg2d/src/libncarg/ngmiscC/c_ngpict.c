/*
 *	$Id: c_ngpict.c,v 1.1 1997-04-11 17:43:40 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngpict
#ifdef NeedFuncProto
(
    int wkid,
    int action
)
#else
(wkid,action)
    int wkid;
    int action;
#endif
{
    NGCALLF(ngpict,NGPICT)(&wkid,&action);
}
