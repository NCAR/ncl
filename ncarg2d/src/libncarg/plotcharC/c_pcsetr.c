/*
 *	$Id: c_pcsetr.c,v 1.1 1997-04-11 17:43:54 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_pcsetr
#ifdef NeedFuncProto
(
    char *whch,
    float rval
)
#else
(whch,rval)
    char *whch;
    float rval;
#endif
{
    float rval2;
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !whch ) {
        fprintf( stderr, "c_pcsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    rval2 = rval;
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(pcsetr,PCSETR)(whch2,&rval2,len);
}
