/*
 *      $Id: c_mdname.c,v 1.3 2008-07-23 16:16:50 haley Exp $
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


#include <ncarg/ncargC.h>

char *c_mdname
#ifdef NeedFuncProto
(
    int iain
)
#else
(iain)
    int iain;
#endif
{
    int len=64;
    static char buff[65];
#if defined(cray)
    extern NGstring NGCALLF(mdname,MDNAME)(_fcd,int*);
    _fcd ft_str;
    ft_str=_cptofcd(buff,len);
    NGCALLF(mdname,MDNAME)(ft_str,&iain);
    strcpy(buff,_fcdtocp(ft_str));
#else
#if defined(AbsoftProFortran)
    extern NGstring NGCALLF(mdname,MDNAME)(char*,int*,int);
    NGCALLF(mdname,MDNAME)(buff,&iain,len);
#else
    extern NGstring NGCALLF(mdname,MDNAME)(char*,int,int*);
    NGCALLF(mdname,MDNAME)(buff,len,&iain);
#endif
#endif
    buff[c_icloem(buff)]='\0';
    return(buff);
}
