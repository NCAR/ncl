/*
 *	$Id: c_sfseti.c,v 1.1 1997-04-11 17:44:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_sfseti
#ifdef NeedFuncProto
(
    char *cnp,
    int ivp
)
#else
(cnp,ivp)
    char *cnp;
    int ivp;
#endif
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_sfseti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(sfseti,SFSETI)(cnp2,&ivp,len);
}
