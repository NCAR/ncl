/*
 *      $Id: c_tdgeti.c,v 1.1 1997-06-30 21:47:25 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdgeti
#ifdef NeedFuncProto
(
    char *pnam,
    int *ival
)
#else
(pnam,ival)
    char *pnam;
    int *ival;
#endif
{
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_tdgeti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(tdgeti,TDGETI)(pnam2,ival,len);
}
