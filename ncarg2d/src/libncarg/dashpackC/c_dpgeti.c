/*
 *	$Id: c_dpgeti.c,v 1.1 1997-04-11 17:41:43 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpgeti
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
    if ( !pnam ) {
        fprintf( stderr, "c_dpgeti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(dpgeti,DPGETI)(pnam2,ival,len);
}
