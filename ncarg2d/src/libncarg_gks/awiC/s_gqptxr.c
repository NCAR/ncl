/*
 *  $Id: s_gqptxr.c,v 1.3 2000-08-01 14:35:53 haley Exp $
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
 *  Inquire predefined text representation  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqptxr,GQPTXR)(Gint*,Gint*,Gint*,Gint*,Gtext_prec*,
                                   Gfloat*,Gfloat*,Gint*);

void ginq_pred_text_rep
#ifdef NeedFuncProto
(
    Gint         ws_type,   /* workstation type         */
    Gint         ind,       /* predefined index         */
    Gint         *err_ind,  /* OUT error indicator      */
    Gtext_bundle *text_rep  /* OUT predefined text rep. */
)
#else
( ws_type, ind, err_ind, text_rep )
    Gint         ws_type;
    Gint         ind;
    Gint         *err_ind;
    Gtext_bundle *text_rep;
#endif
{
    Gfloat charxp, charsp;

    NGCALLF(gqptxr,GQPTXR)(&ws_type,&ind,err_ind,
                           &text_rep->text_font_prec.font,
                           &text_rep->text_font_prec.prec,
                           &charxp,&charsp,&text_rep->colr_ind);

    text_rep->char_expan = (Gdouble)charxp;
    text_rep->char_space = (Gdouble)charsp;
}
