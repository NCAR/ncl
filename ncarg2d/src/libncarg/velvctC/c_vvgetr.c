/*
 *	$Id: c_vvgetr.c,v 1.1 1997-04-11 17:45:17 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_vvgetr
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
         fprintf( stderr, "c_vvgetr:  illegal parameter name (NULL)\n" );
         return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(vvgetr,VVGETR)(whch2,rval,len);
}
