/*
 *	$Id: c_lbsetr.c,v 1.1 1997-04-11 17:43:33 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_lbsetr
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
        fprintf( stderr, "c_lbsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    rval2 = rval;
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(lbsetr,LBSETR)(whch2,&rval2,len);
}
