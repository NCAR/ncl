/*
 *  $Id: s_gqpplr.c,v 1.3 2000-08-01 14:35:53 haley Exp $
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
 *  Inquire predefined polyline representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpplr,GQPPLR)(Gint*,Gint*,Gint*,Gint*,Gfloat*,Gint*);

void ginq_pred_line_rep
#ifdef NeedFuncProto
(
    Gint         ws_type,           /* workstation type             */
    Gint         ind,               /* predefined index             */
    Gint         *err_ind,          /* OUT error indicator          */
    Gline_bundle *line_rep  /* OUT predefined polyline rep. */
)
#else
( ws_type, ind, err_ind, line_rep )
    Gint         ws_type;
    Gint         ind;
    Gint         *err_ind;
    Gline_bundle *line_rep;
#endif
{
    Gfloat width;

    NGCALLF(gqpplr,GQPPLR)(&ws_type,&ind,err_ind,&line_rep->type,
                           &width,&line_rep->colr_ind);
    line_rep->width = (Gdouble) width;
}
