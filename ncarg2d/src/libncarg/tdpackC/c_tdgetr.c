/*
 *      $Id: c_tdgetr.c,v 1.1 1997-06-30 21:47:27 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdgetr
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
    if( !pnam ) {
        fprintf( stderr, "c_tdgetr:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(tdgetr,TDGETR)(pnam2,rval,len);
}
