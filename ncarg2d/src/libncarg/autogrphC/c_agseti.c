/*
 *	$Id: c_agseti.c,v 1.1 1997-04-11 17:40:38 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_agseti
#ifdef NeedFuncProto
(
    char *tpid,
    int iusr
)
#else
(tpid,iusr)
    char *tpid;
    int iusr;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_agseti:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(agseti,AGSETI)(tpid2,&iusr,len);
}
