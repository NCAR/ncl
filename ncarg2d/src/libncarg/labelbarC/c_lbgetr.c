/*
 *	$Id: c_lbgetr.c,v 1.1 1997-04-11 17:43:31 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_lbgetr
#ifdef NeedFuncProto
(
    char *whch,
    float *rval
)
#else
(whch,rval)
    char *whch;
    float *rval;
#endif
{
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) {
        fprintf( stderr, "c_lbgetr:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(lbgetr,LBGETR)(whch2,rval,len);
}
