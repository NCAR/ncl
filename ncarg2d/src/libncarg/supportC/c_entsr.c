/*
 *	$Id: c_entsr.c,v 1.1 1997-04-11 17:44:54 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_entsr
#ifdef NeedFuncProto
(
    int *irold,
    int irnew
)
#else
(irold,irnew)
    int *irold;
    int irnew;
#endif
{
    NGCALLF(entsr,ENTSR)( irold, &irnew);
}
