/*
 *	$Id: c_pcgetr.c,v 1.1 1997-04-11 17:43:49 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_pcgetr
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
        fprintf( stderr, "c_pcgetr:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(pcgetr,PCGETR)(whch2,rval,len);
}
