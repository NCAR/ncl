/*
 *	$Id: c_stsetr.c,v 1.1 1997-04-11 17:44:51 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_stsetr
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
        fprintf( stderr, "c_stsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    rval2 = rval;

    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(stsetr,STSETR)(whch2,&rval2,len);
}
