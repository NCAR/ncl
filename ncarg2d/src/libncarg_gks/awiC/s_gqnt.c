/*
 *	$Id: s_gqnt.c,v 1.5 2000-08-22 15:09:00 haley Exp $
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
 *  Inquire normalization transformation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqnt,GQNT)(Gint*,Gint*,Gfloat*,Gfloat*);

void ginq_norm_tran
#ifdef NeedFuncProto
(
    Gint  num,       /* normalization transformation number */   
    Gint  *err_ind,  /* OUT error indicator                 */
    Gtran *norm_tran /* OUT normalization tranformation     */
)
#else
( num, err_ind, norm_tran )
    Gint  num;
    Gint  *err_ind;
    Gtran *norm_tran;
#endif
{
    Gfloat win[4], vp[4];
    NGCALLF(gqnt,GQNT)(&num,err_ind, win, vp);
    norm_tran->win.x_min = (Gfloat)win[0];
    norm_tran->win.x_max = (Gfloat)win[1];
    norm_tran->win.y_min = (Gfloat)win[2];
    norm_tran->win.y_max = (Gfloat)win[3];
    norm_tran->vp.x_min = (Gfloat)vp[0];
    norm_tran->vp.x_max = (Gfloat)vp[1];
    norm_tran->vp.y_min = (Gfloat)vp[2];
    norm_tran->vp.y_max = (Gfloat)vp[3];
    return;
}
