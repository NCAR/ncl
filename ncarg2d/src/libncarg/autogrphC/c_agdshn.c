/*
 *	$Id: c_agdshn.c,v 1.1 1997-04-11 17:40:30 haley Exp $
 */
#include <ncarg/ncargC.h>

char *c_agdshn
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
    extern NGstring NGCALLF(agdshn,AGDSHN)();
#if defined(cray)
    _fcd ft_str;

    ft_str = _cptofcd(buff,len);
    NGCALLF(agdshn,AGDSHN)(ft_str,&idsh);
    strcpy( buff, _fcdtocp(ft_str));
#else
    NGCALLF(agdshn,AGDSHN)(buff,len,&idsh);
#endif
    buff[c_icloem(buff)] = '\0';
    return(buff);
}
