/*
 *	$Id: c_ngsetr.c,v 1.1 1997-04-11 17:43:44 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngsetr
#ifdef NeedFuncProto
(
    char *pnam,
    float rval
)
#else
(pnam,rval)
    char *pnam;
    float rval;
#endif
{
    float rval2;
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_ngsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    rval2 = rval;
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(ngsetr,NGSETR)(pnam2,&rval2,len);
}
