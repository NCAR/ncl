/*
 *	$Id: s_gqtxx.c,v 1.4 2000-08-22 15:09:12 haley Exp $
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
 * Inquire text extent
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxx,GQTXX)(Gint*,const Gfloat*,const Gfloat*,NGstring,
                                 Gint*,Gfloat*,Gfloat*,Gfloat x[4],
                                 Gfloat y[4],int);

void ginq_text_extent
#ifdef NeedFuncProto
(
    Gint         ws_id,    /* workstation identifier      */
    const Gpoint *pos,     /* text position               */
    const char   *str,     /* text string                 */
    Gint         *err_ind, /* OUT error indicator         */
    Gtext_extent *extent   /* OUT concatentation point and
							  text extent parallelogram   */
)
#else
( ws_id, pos, str, err_ind, extent )
    Gint         ws_id;
    Gpoint       *pos;
    char         *str;
    Gint         *err_ind;
    Gtext_extent *extent;
#endif
{
    int i, len;
    Gfloat x[4], y[4];
    NGstring str2;

    len = NGSTRLEN(str);
    str2 = NGCstrToFstr(str,len);
    NGCALLF(gqtxx,GQTXX)(&ws_id,&pos->x,&pos->y,str2,err_ind,
                         &extent->concat_point.x,&extent->concat_point.y,
                         x,y,len);
    for( i = 0; i < 4; i++ ) {
        extent->paral[i].x = (Gfloat) x[i];
        extent->paral[i].y = (Gfloat) y[i];
    }
}
