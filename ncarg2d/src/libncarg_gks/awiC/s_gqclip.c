/*
 *	$Id: s_gqclip.c,v 1.2 2000-07-12 17:06:11 haley Exp $
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

/*
 *  Inquire clipping indicator  
 */

#include <ncarg/gks.h>

void ginq_clip
#ifdef NeedFuncProto
(
    Gint  *err_ind,        /* OUT error indicator                         */
    Gclip *clip_ind_rect   /* OUT current clippig indicator and rectangle */
)
#else
( err_ind, clip_ind_rect )
    Gint  *err_ind;
    Gclip *clip_ind_rect;
#endif
{
    Gfloat clrect[4];

    NGCALLF(gqclip,GQCLIP)(err_ind,&clip_ind_rect->clip_ind,clrect);

    clip_ind_rect->clip_rect.x_min = (Gfloat) clrect[0];
    clip_ind_rect->clip_rect.x_max = (Gfloat) clrect[1];
    clip_ind_rect->clip_rect.y_min = (Gfloat) clrect[2];
    clip_ind_rect->clip_rect.y_max = (Gfloat) clrect[3];
}
