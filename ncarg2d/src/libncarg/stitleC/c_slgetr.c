/*
 *	$Id: c_slgetr.c,v 1.1 1997-04-11 17:44:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_slgetr
#ifdef NeedFuncProto
(
    char *pa,
    float *rval
)
#else
(pa,rval)
    char *pa;
    float *rval;
#endif
{
    NGstring pa2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pa ) {
        fprintf( stderr, "c_slgetr:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pa);
    pa2 = NGCstrToFstr(pa,len);
    NGCALLF(slgetr,SLGETR)(pa2,rval,len);
}
