/*
 *      $Id: c_tdseti.c,v 1.1 1997-06-30 21:47:47 kennison Exp $
 */
#include <ncarg/ncargC.h>

void c_tdseti
#ifdef NeedFuncProto
(
    char *pnam,
    int ival
)
#else
(pnam,ival)
    char *pnam;
    int ival;
#endif
{
    NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_tdseti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(tdseti,TDSETI)(pnam2,&ival,len);
}
