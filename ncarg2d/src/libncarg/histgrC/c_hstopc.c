/*
 *	$Id: c_hstopc.c,v 1.1 1997-04-11 17:43:16 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_hstopc
#ifdef NeedFuncProto
(
    char *iopt,
    char *string,
    int number,
    int ilch
)
#else
(iopt,string,number,ilch)
    char *iopt;
    char *string;
    int number;
    int ilch;
#endif
{
    NGstring iopt2;
    NGstring string2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !iopt ) {
        fprintf( stderr, "c_hstopc:  illegal parameter name (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(iopt);
    len2 = NGSTRLEN(string);
    iopt2 = NGCstrToFstr(iopt,len1);
    string2 = NGCstrToFstr(string,len2);
    NGCALLF(hstopc,HSTOPC)(iopt2,string2,&number,&ilch,len1,len2);
}
