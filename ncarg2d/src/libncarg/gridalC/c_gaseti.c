/*
 *	$Id: c_gaseti.c,v 1.1 1997-04-11 17:42:52 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gaseti
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
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gaseti:  illegal parameter string (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(gaseti,GASETI)(pnam2,&ival,len);
}
