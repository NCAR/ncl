/*
 *	$Id: c_wmseti.c,v 1.1 1997-04-11 17:45:30 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_wmseti
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
    if ( !cnp ) {
        fprintf( stderr, "c_wmseti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(wmseti,WMSETI)(cnp2,&ivp,len);
}
