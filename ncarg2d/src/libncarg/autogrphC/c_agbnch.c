/*
 *	$Id: c_agbnch.c,v 1.1 1997-04-11 17:40:29 haley Exp $
 */
#include <ncarg/ncargC.h>

char *c_agbnch
#ifdef NeedFuncProto
(
    int idsh
)
#else
(idsh)
    int idsh;
#endif
{
    int len = 16;
    static char buff[17];
    extern NGstring NGCALLF(agbnch,AGBNCH)();
#if defined(cray)
    _fcd ft_str;

    ft_str = _cptofcd(buff,len);
    NGCALLF(agbnch,AGBNCH)(ft_str,&idsh);
    strcpy( buff, _fcdtocp(ft_str));
#else
    NGCALLF(agbnch,AGBNCH)(buff,len,&idsh);
#endif
    buff[c_icloem(buff)] = '\0';
    return(buff);
}
