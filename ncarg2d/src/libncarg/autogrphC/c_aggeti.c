/*
 *	$Id: c_aggeti.c,v 1.1 1997-04-11 17:40:34 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_aggeti
#ifdef NeedFuncProto
(
    char *tpid,
    int *iusr
)
#else
(tpid,iusr)
    char *tpid;
    int *iusr;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggeti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(aggeti,AGGETI)(tpid2,iusr,len);
}
