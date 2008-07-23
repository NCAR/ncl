/*
 *	$Id: c_pcpnwi.c,v 1.7 2008-07-23 15:46:49 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdlib.h>
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
    char *buff;
#if defined(cray)
    _fcd cftwhch, ft_str;
    extern NGstring NGCALLF(pcpnwi,PCPNWI)(_fcd,_fcd,int*);

    buff = (char *)malloc((len+1)*sizeof(char));
    ft_str = NGCstrToFstr(buff,len);
    cftwhch = NGCstrToFstr(whch,NGSTRLEN(whch));
    NGCALLF(pcpnwi,PCPNWI)(ft_str,cftwhch,&ipai);
    strcpy( buff, NGFstrToCstr(ft_str));
#else
#if defined(AbsoftProFortran)
    extern NGstring NGCALLF(pcpnwi,PCPNWI)(char*,char*,int*,int,int);
    NGCALLF(pcpnwi,PCPNWI)(buff,whch,&ipai,len,NGSTRLEN(whch));
#else
    extern NGstring NGCALLF(pcpnwi,PCPNWI)(char*,int,char*,int*,int);
    NGCALLF(pcpnwi,PCPNWI)(buff,len,whch,&ipai,NGSTRLEN(whch));
#endif
#endif
    buff[len] = '\0';
    return(buff);
}
