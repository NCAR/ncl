/*
 *	$Id: c_agdshn.c,v 1.5 2002-02-23 02:49:37 haley Exp $
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
