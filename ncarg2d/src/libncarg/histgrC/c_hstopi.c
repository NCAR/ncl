/*
 *	$Id: c_hstopi.c,v 1.1 1997-04-11 17:43:16 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_hstopi
#ifdef NeedFuncProto
(
    char *string,
    int param1,
    int param2,
    int *icol,
    int lcol
)
#else
(string, param1,param2,icol,lcol)
    char *string;
    int param1;
    int param2;
    int *icol;
    int lcol;
#endif
{
    NGstring string2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !string ) {
        fprintf( stderr, "c_hstopi:  illegal parameter name (NULL)\n" );
        return;
    }
    len = NGSTRLEN(string);
    string2 = NGCstrToFstr(string,len);
    NGCALLF(hstopi,HSTOPI)(string2,&param1,&param2,icol,&lcol,len);
}
