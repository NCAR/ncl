/*
 *	$Id: c_pcpnwi.c,v 1.2 2000-07-12 16:25:04 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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
