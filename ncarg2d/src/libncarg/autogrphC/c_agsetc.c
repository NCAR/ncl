/*
 *	$Id: c_agsetc.c,v 1.1 1997-04-11 17:40:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agsetc
#ifdef NeedFuncProto
(
    char *tpid,
    char *cusr
)
#else
(tpid,cusr)
    char *tpid;
    char *cusr;
#endif
{
    NGstring tpid2;
    NGstring cusr2;
    int len1, len2;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_agsetc:  illegal parameter string (NULL)\n" );
        return;
    }
/*
 * Make sure return string is not NULL
 */
    if( !cusr ) { 
        fprintf( stderr, "c_agsetc:  illegal return string (NULL)\n" );
        return;
    }
    len1 = NGSTRLEN(tpid);
    len2 = NGSTRLEN(cusr);
    tpid2 = NGCstrToFstr(tpid,len1);
    cusr2 = NGCstrToFstr(cusr,len2);
    NGCALLF(agsetc,AGSETC)(tpid2,cusr2,len1,len2);
}
