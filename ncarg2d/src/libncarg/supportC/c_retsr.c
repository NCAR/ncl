/*
 *	$Id: c_retsr.c,v 1.1 1997-04-11 17:45:02 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_retsr
#ifdef NeedFuncProto
(
    int irold
)
#else
(irold)
    int irold;
#endif
{
    NGCALLF(retsr,RETSR)(&irold);
}
