/*
 *  $Id: s_gqpcr.c,v 1.4 2000-08-22 15:09:04 haley Exp $
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
 *  Inquire predefined colour representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpcr,GQPCR)(Gint*,Gint*,Gint*,Gfloat*,Gfloat*,Gfloat*);

void ginq_pred_colr_rep
#ifdef NeedFuncProto
(
    Gint      ws_type,  /* workstation identifier     */
    Gint      ind,      /* predefined index           */
    Gint      *err_ind, /* OUT error indicator        */
    Gcolr_rep *colr_rep /* OUT predefined colour rep. */
)
#else
( ws_type, ind, err_ind, colr_rep )
    Gint      ws_type;
    Gint      ind;
    Gint      *err_ind;
    Gcolr_rep *colr_rep;
#endif
{
    NGCALLF(gqpcr,GQPCR)(&ws_type,&ind,err_ind,
                         &colr_rep->rgb.red,
                         &colr_rep->rgb.green,
                         &colr_rep->rgb.blue);
}
