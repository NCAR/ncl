/*
 *	$Id: c_agbnch.c,v 1.6 2008-07-23 16:16:40 haley Exp $
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
#if defined(cray)
    _fcd ft_str;
    extern NGstring NGCALLF(agbnch,AGBNCH)(_fcd,int*);

    ft_str = _cptofcd(buff,len);
    NGCALLF(agbnch,AGBNCH)(ft_str,&idsh);
    strcpy( buff, _fcdtocp(ft_str));
#else
#if defined(AbsoftProFortran)
    extern NGstring NGCALLF(agbnch,AGBNCH)(char*,int*,int);
    NGCALLF(agbnch,AGBNCH)(buff,&idsh,len);
#else
    extern NGstring NGCALLF(agbnch,AGBNCH)(char*,int,int*);
    NGCALLF(agbnch,AGBNCH)(buff,len,&idsh);
#endif
#endif
    buff[c_icloem(buff)] = '\0';
    return(buff);
}
