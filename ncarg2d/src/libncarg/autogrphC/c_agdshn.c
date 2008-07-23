/*
 *	$Id: c_agdshn.c,v 1.6 2008-07-23 16:16:40 haley Exp $
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
#if defined(cray)
    extern NGstring NGCALLF(agdshn,AGDSHN)(_fcd,int*);
    _fcd ft_str;

    ft_str = _cptofcd(buff,len);
    NGCALLF(agdshn,AGDSHN)(ft_str,&idsh);
    strcpy( buff, _fcdtocp(ft_str));
#else
#if defined(AbsoftProFortran)
    extern NGstring NGCALLF(agdshn,AGDSHN)(char*,int*,int);
    NGCALLF(agdshn,AGDSHN)(buff,&idsh,len);
#else
    extern NGstring NGCALLF(agdshn,AGDSHN)(char*,int,int*);
    NGCALLF(agdshn,AGDSHN)(buff,len,&idsh);
#endif
#endif
    buff[c_icloem(buff)] = '\0';
    return(buff);
}
