/*
 *	$Id: c_vvsetr.c,v 1.1 1997-04-11 17:45:19 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_vvsetr
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
         fprintf( stderr, "c_vvsetr:  illegal parameter name (NULL)\n" );
         return;
    }
    len = NGSTRLEN(whch);
    rval2 = rval;
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(vvsetr,VVSETR)(whch2,&rval2,len);
}
