/*
 *	$Id: c_icloem.c,v 1.1 1997-04-11 17:44:55 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_icloem
#ifdef NeedFuncProto
(
    char *messg
)
#else
(messg)
    char *messg;
#endif
{
    NGstring messg2;
    int len;
    extern int NGCALLF(icloem,ICLOEM)();
/*
 * Make sure message is not NULL
 */
    if( !messg ) { 
        fprintf( stderr, "c_icloem:  illegal message (NULL)\n" );
        return(0);
    }
    len = NGSTRLEN(messg);
    messg2 = NGCstrToFstr(messg,len);
    return(NGCALLF(icloem,ICLOEM)(messg2,len));
}

