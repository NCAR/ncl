/*
 *      $Id: c_cttdbm.c,v 1.2 2004-03-27 00:25:45 kennison Exp $
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

extern void NGCALLF(cttdbm,CTTDBM)(int*,int*,int*,int*,int*,int*,int*,int*);

void c_cttdbm
#ifdef NeedFuncProto
(
    int ihbx,
    int iebx,
    int iwbx,
    int iubx,
    int ihba,
    int ieba,
    int iwba,
    int iuba
)
#else
(ihbx,iebx,iwbx,iubx,ihba,ieba,iwba,iuba)
    int ihbx;
    int iebx;
    int iwbx;
    int iubx;
    int ihba;
    int ieba;
    int iwba;
    int iuba;
#endif
{
    NGCALLF(cttdbm,CTTDBM)(&ihbx,&iebx,&iwbx,&iubx,&ihba,&ieba,&iwba,&iuba);
}
