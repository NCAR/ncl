/*
 *	$Id: c_sfsetr.c,v 1.1 1997-04-11 17:44:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfsetr
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
    int len;
    NGstring cnp2;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_sfsetr:  illegal parameter name (NULL)\n");
        return;
    }

    rvp2 = rvp;

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(sfsetr,SFSETR)(cnp2,&rvp2,len);
}
