/*
 *	$Id: c_sfgetp.c,v 1.1 1997-04-11 17:44:08 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfgetp
#ifdef NeedFuncProto
(
    int idp[8][8]
)
#else
(idp)
    int idp[8][8];
#endif
{
    NGCALLF(sfgetp,SFGETP)(idp);
}
