/*
 *	$Id: c_nggeti.c,v 1.1 1997-04-11 17:43:38 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_nggeti
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
    if ( !pnam) {
        fprintf( stderr, "c_nggeti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(nggeti,NGGETI)(pnam2,ival,len);
}
