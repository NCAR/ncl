/*
 *	$Id: c_perim3.c,v 1.2 2000-07-12 16:26:48 haley Exp $
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

void c_perim3 
#ifdef NeedFuncProto
(
    int magr1,
    int mini1,
    int magr2,
    int mini2,
    int iwhich,
    float var
)
#else
 (magr1,mini1,magr2,mini2,iwhich,var)
    int magr1;
    int mini1;
    int magr2;
    int mini2;
    int iwhich;
    float var;
#endif
{
    float var2;
    var2 = var;
    NGCALLF(perim3,PERIM3)(&magr1,&mini1,&magr2,&mini2,&iwhich,&var2);
}
