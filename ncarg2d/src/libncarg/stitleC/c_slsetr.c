/*
 *	$Id: c_slsetr.c,v 1.1 1997-04-11 17:44:46 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_slsetr
#ifdef NeedFuncProto
(
    char *pa,
    float rval
)
#else
(pa,rval)
    char *pa;
    float rval;
#endif
{
    float rval2;
    NGstring pa2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pa ) {
        fprintf( stderr, "c_slsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    rval2 = rval;
    len = NGSTRLEN(pa);
    pa2 = NGCstrToFstr(pa,len);
    NGCALLF(slsetr,SLSETR)(pa2,&rval2,len);
}
