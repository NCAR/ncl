/*
 *	$Id: c_nglogo.c,v 1.1 2002-04-04 22:05:31 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2002                                    *
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

extern void NGCALLF(nglogo,NGLOGO)(int*, float*, float*, float*, int*, int*, int*);

void c_nglogo
#ifdef NeedFuncProto
(
    int iwk,
    float x,
    float y,
    float size,
    int itype,
    int icol1,
    int icol2
)
#else
(iwk,x,y,size,itype,icol1,icol2)
    int iwk;
    float x;
    float y;
    float size;
    int itype;
    int icol1;
    int icol2;
#endif
{
    NGCALLF(nglogo,NGLOGO)(&iwk,&x,&y,&size,&itype,&icol1,&icol2);
}
