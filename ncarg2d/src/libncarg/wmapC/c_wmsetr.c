/*
 *	$Id: c_wmsetr.c,v 1.1 1997-04-11 17:45:31 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmsetr
#ifdef NeedFuncProto
(
    char *cnp,
    float rvp
)
#else
(cnp,rvp)
    char *cnp;
    float rvp;
#endif
{
    float rvp2;
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) { 
        fprintf( stderr, "c_wmsetr:  illegal parameter name (NULL)\n" );
        return;
    }

    rvp2 = rvp;

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(wmsetr,WMSETR)(cnp2,&rvp2,len);
}
