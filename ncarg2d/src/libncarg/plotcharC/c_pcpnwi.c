/*
 *	$Id: c_pcpnwi.c,v 1.1 1997-04-11 17:43:52 haley Exp $
 */
#include <ncarg/ncargC.h>

char *c_pcpnwi
#ifdef NeedFuncProto
(
    char *whch,
    int ipai
)
#else
(whch,ipai)
    char *whch;
    int ipai;
#endif
{
    int len = 16;
    char buff[17];
    extern NGstring NGCALLF(pcpnwi,PCPNWI)();
#if defined(cray)
    _fcd cftwhch, ft_str;

    ft_str = NGCstrToFstr(buff,len);
    cftwhch = NGCstrToFstr(whch,NGSTRLEN(whch));
    NGCALLF(pcpnwi,PCPNWI)(ft_str,cftwhch,&ipai);
    strcpy( buff, NGFstrToCstr(ft_str));
#else
    NGCALLF(pcpnwi,PCPNWI)(buff,len,whch,&ipai,NGSTRLEN(whch));
#endif
    buff[len] = '\0';
    return(buff);
}
