/*
 *	$Id: c_nggetr.c,v 1.1 1997-04-11 17:43:38 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_nggetr
#ifdef NeedFuncProto
(
    char *pnam,
    float *rval
)
#else
(pnam,rval)
    char *pnam;
    float *rval;
#endif
{
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !pnam ) {
        fprintf( stderr, "c_nggetr:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(nggetr,NGGETR)(pnam2,rval,len);
}
