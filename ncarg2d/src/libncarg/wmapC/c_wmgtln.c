/*
 *	$Id: c_wmgtln.c,v 1.2 2000-07-12 16:27:10 haley Exp $
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

int c_wmgtln
#ifdef NeedFuncProto
(
    char *lab,
    int lablen,    
    int ilr
)
#else
(lab,lablen,ilr)
    char *lab,
    int lablen,    
    int ilr
#endif
{
	extern int NGCALLF(wmgtln,WMGTLN)();
    NGstring lab2;
    int len;
    len = NGSTRLEN(lab);
    lab2 = NGCstrToFstr(lab,len);
    return(NGCALLF(wmgtln,WMGTLN)(lab2,&lablen,&ilr,len));
}
