/*
 *	$Id: c_sfsetp.c,v 1.1 1997-04-11 17:44:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfsetp
#ifdef NeedFuncProto
(
    int idp[8][8]
)
#else
(idp)
    int idp[8][8];
#endif
{
    NGCALLF(sfsetp,SFSETP)(idp);
}
