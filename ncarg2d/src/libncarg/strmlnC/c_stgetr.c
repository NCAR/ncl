/*
 *	$Id: c_stgetr.c,v 1.1 1997-04-11 17:44:49 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_stgetr
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
        fprintf( stderr, "c_stgetr:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(stgetr,STGETR)(whch2,rval,len);
}
