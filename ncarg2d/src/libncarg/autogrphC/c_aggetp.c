/*
 *	$Id: c_aggetp.c,v 1.1 1997-04-11 17:40:35 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_aggetp
#ifdef NeedFuncProto
(
    char *tpid,
    float *fura,
    int lura
)
#else
(tpid,fura,lura)
    char *tpid;
    float *fura;
    int lura;
#endif
{
    NGstring tpid2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !tpid ) { 
        fprintf( stderr, "c_aggetp:  illegal parameter string (NULL)\n" );
        return;
    }
    len = NGSTRLEN(tpid);
    tpid2 = NGCstrToFstr(tpid,len);
    NGCALLF(aggetp,AGGETP)(tpid2,fura,&lura,len);
}
