/*
 *	$Id: c_getusv.c,v 1.1 1997-04-11 17:44:21 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_getusv
#ifdef NeedFuncProto
(
    char *vn,
    int *iv
)
#else
(vn,iv)
    char *vn;
    int *iv;
#endif
{
    NGstring vn2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !vn ) {
        fprintf( stderr, "c_getusv:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(vn);
    vn2 = NGCstrToFstr(vn,len);
    NGCALLF(getusv,GETUSV)(vn2,iv,len);
}
