/*
 *	$Id: c_semess.c,v 1.1 1997-04-11 17:45:03 haley Exp $
 */
#include <ncarg/ncargC.h>

char *c_semess
#ifdef NeedFuncProto
(
    int itrim
)
#else
(itrim)
    int itrim;
#endif
{
    int len = 113;
    static char buff[114];
#if defined(cray)
    extern _fcd NGCALLF(semess,SEMESS)();
    _fcd ft_str;

    ft_str = _cptofcd(buff,len);
    NGCALLF(semess,SEMESS)(ft_str,&itrim);
    strcpy( buff, _fcdtocp(ft_str));
#else
    NGCALLF(semess,SEMESS)(buff,len,&itrim);
#endif
    buff[c_icloem(buff)] = '\0';
    return(buff);
}
