/*
 *	$Id: c_pcpnwi.c,v 1.6 2008-07-23 15:07:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
