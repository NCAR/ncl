/*
 *	$Id: c_pcdlsc.c,v 1.1 1997-04-11 17:43:48 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_pcdlsc 
#ifdef NeedFuncProto
(
    int ifci
)
#else
 (ifci)
    int ifci;
#endif
{
    NGCALLF(pcdlsc,PCDLSC)(&ifci);
}

