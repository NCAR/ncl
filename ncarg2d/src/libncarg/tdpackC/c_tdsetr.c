/*
 *      $Id: c_tdsetr.c,v 1.1 1997-06-30 21:47:49 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdsetr
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
        fprintf( stderr, "c_tdsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    rval2 = rval;
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(tdsetr,TDSETR)(pnam2,&rval2,len);
}
