/*
 *	$Id: c_agsetf.c,v 1.1 1997-04-11 17:40:37 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agsetf
#ifdef NeedFuncProto
(
    char *tpid,
    float fusr
)
#else
(tpid,fusr)
    char *tpid;
    float fusr;
#endif
{
    float fusr2;
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_agsetf:  illegal parameter string (NULL)\n" );
        return;
    }

    fusr2 = fusr;

    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(agsetf,AGSETF)(tpid2,&fusr2,len);
}
