/*
 *      $Id: c_mdfnme.c,v 1.3 2008-07-23 16:16:48 haley Exp $
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

char *c_mdfnme
#ifdef NeedFuncProto
(
    int iain,
    int ilvl
)
#else
(iain,ilvl)
    int iain;
    int ilvl;
#endif
{
    int len=128;
    static char buff[129];
#if defined(cray)
    extern NGstring NGCALLF(mdfnme,MDFNME)(_fcd,int*,int*);
    _fcd ft_str;
    ft_str=_cptofcd(buff,len);
    NGCALLF(mdfnme,MDFNME)(ft_str,&iain,&ilvl);
    strcpy(buff,_fcdtocp(ft_str));
#if defined(AbsoftProFortran)
    extern NGstring NGCALLF(mdfnme,MDFNME)(char *,int*,int*,int);
    NGCALLF(mdfnme,MDFNME)(buff,&iain,&ilvl,len);
#else
    extern NGstring NGCALLF(mdfnme,MDFNME)(char *,int,int*,int*);
    NGCALLF(mdfnme,MDFNME)(buff,len,&iain,&ilvl);
#endif
#endif
    buff[c_icloem(buff)]='\0';
    return(buff);
}
