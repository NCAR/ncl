/*
 *	$Id: c_gageti.c,v 1.1 1997-04-11 17:42:42 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gageti
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
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gageti:  illegal parameter string (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(gageti,GAGETI)(pnam2,ival,len);
}
