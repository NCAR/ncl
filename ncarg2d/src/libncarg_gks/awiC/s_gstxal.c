/*
 *	$Id: s_gstxal.c,v 1.4 2000-08-22 15:09:23 haley Exp $
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

/*
 *  Set text alignment  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gstxal,GSTXAL)(const Ghor_text_align*,
                                   const Gvert_text_align*);

void gset_text_align
#ifdef NeedFuncProto
(
    const Gtext_align *text_align  /* text alignment */
)
#else
( text_align )
    Gtext_align *text_align;
#endif
{
    NGCALLF(gstxal,GSTXAL)(&text_align->hor,&text_align->vert);
}
