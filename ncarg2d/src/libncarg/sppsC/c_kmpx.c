/*
 *	$Id: c_kmpx.c,v 1.1 1997-04-11 17:44:23 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_kmpx
#ifdef NeedFuncProto
(
    int ix
)
#else
(ix)
    int ix;
#endif
{
	return(NGCALLF(kmpx,KMPX)(&ix));
}
