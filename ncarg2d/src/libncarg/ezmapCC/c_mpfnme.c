/*
 *      $Id: c_mpfnme.c,v 1.1 2001-10-10 02:51:36 haley Exp $
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

char *c_mpfnme
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
	extern NGstring NGCALLF(mpfnme,MPFNME)(_fcd,int*,int*);
    _fcd ft_str;
    ft_str=_cptofcd(buff,len);
    NGCALLF(mpfnme,MPFNME)(ft_str,&iain,&ilvl);
    strcpy(buff,_fcdtocp(ft_str));
#else
	extern NGstring NGCALLF(mpfnme,MPFNME)(char*,int,int*,int*);
    NGCALLF(mpfnme,MPFNME)(buff,len,&iain,&ilvl);
#endif
    buff[c_icloem(buff)]='\0';
    return(buff);
}
