/*
 *      $Id: c_mdname.c,v 1.1 2001-10-10 02:51:28 haley Exp $
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
	extern NGstring NGCALLF(mdname,MDNAME)(char*,int,int*);
    NGCALLF(mdname,MDNAME)(buff,len,&iain);
#endif
    buff[c_icloem(buff)]='\0';
    return(buff);
}
