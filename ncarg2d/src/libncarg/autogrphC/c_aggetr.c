/*
 *	$Id: c_aggetr.c,v 1.1 1997-04-11 17:40:35 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_aggetr
#ifdef NeedFuncProto
(
    char *tpid,
    float *fusr
)
#else
(tpid,fusr)
    char *tpid;
    float *fusr;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggetr:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(aggetr,AGGETR)(tpid2,fusr,len);
}
