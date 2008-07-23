/*
 *	$Id: c_semess.c,v 1.5 2008-07-23 16:17:05 haley Exp $
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
    extern NGstring NGCALLF(semess,SEMESS)(_fcd,int*);
    _fcd ft_str;

    ft_str = _cptofcd(buff,len);
    NGCALLF(semess,SEMESS)(ft_str,&itrim);
    strcpy( buff, _fcdtocp(ft_str));
#else
    extern NGstring NGCALLF(semess,SEMESS)(char*,int,int*);
    NGCALLF(semess,SEMESS)(buff,len,&itrim);
#endif
    buff[c_icloem(buff)] = '\0';
    return(buff);
}
