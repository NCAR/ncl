/*
 *	$Id: c_sfgetr.c,v 1.1 1997-04-11 17:44:08 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfgetr
#ifdef NeedFuncProto
(
    char *cnp,
    float *rvp
)
#else
(cnp,rvp)
    char *cnp;
    float *rvp;
#endif
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_sfgetr:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(sfgetr,SFGETR)(cnp2,rvp,len);
}
