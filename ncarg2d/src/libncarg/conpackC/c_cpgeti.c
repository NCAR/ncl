/*
 *   $Id: c_cpgeti.c,v 1.1 1997-04-11 17:41:04 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpgeti
#ifdef NeedFuncProto
(
    char *whch,
    int *ival
)
#else
(whch,ival)
    char *whch;
    int *ival;
#endif
{
    NGstring whch2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !whch) {
        fprintf( stderr, "c_cpgeti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(whch);
    whch2 = NGCstrToFstr(whch,len);
    NGCALLF(cpgeti,CPGETI)(whch2,ival,len);
}
