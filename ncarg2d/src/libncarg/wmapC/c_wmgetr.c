/*
 *	$Id: c_wmgetr.c,v 1.1 1997-04-11 17:45:26 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmgetr
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
        fprintf( stderr, "c_wmgetr:  illegal parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(wmgetr,WMGETR)(cnp2,rvp,len);
}
