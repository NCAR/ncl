/*
 *  $Id: s_gqtxf.c,v 1.3 2000-08-01 14:35:54 haley Exp $
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
 *  Inquire text facilities  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqtxf,GQTXF)(Gint*,int*,Gint*,Gint*,Gint*,Gtext_prec*,
                                 Gint*,Gfloat*,Gfloat*,Gint*,Gfloat*,Gfloat*,
                                 Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_text_facs
#ifdef NeedFuncProto
(
    Gint       ws_type,             /* workstation type                            */
    Gint       num_elems_appl_list, /* length of application list                  */
    Gint       start_pos,           /* starting position                           */
    Gint       *err_ind,            /* OUT error indicator                         */
    Gtext_facs *text_facs,          /* OUT text facilities                         */
    Gint       *length_list         /* OUT length of text font and precision list 
                                       in GKS                                      */
)
#else
(ws_type,num_elems_appl_list,start_pos,err_ind,text_facs,
                    length_list )
    Gint       ws_type;
    Gint       num_elems_appl_list;
    Gint       start_pos;
    Gint       *err_ind;
    Gtext_facs *text_facs;
    Gint       *length_list;
#endif
{
    Gfloat minchh, maxchh, minchx, maxchx;
    int num, i, j = 0, beg_pos, end_pos;
/*
 * Position "n" in C is position "n+1" in Fortran
 */
    beg_pos = start_pos + 1;
/*
 * Get first element
 */
    NGCALLF(gqtxf,GQTXF)(&ws_type,&beg_pos,err_ind,length_list,
           &text_facs->font_precs[j].font,&text_facs->font_precs[j].prec,
           &text_facs->num_char_hts,&minchh,&maxchh,
           &text_facs->num_char_expans,&minchx,&maxchx,
           &text_facs->num_pred_inds);
    text_facs->min_char_ht = (Gdouble)minchh;
    text_facs->max_char_ht = (Gdouble)maxchh;
    text_facs->min_char_expan = (Gdouble)minchx;
    text_facs->max_char_expan = (Gdouble)maxchx;
    text_facs->num_font_precs = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        text_facs->num_font_precs = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqtxf,GQTXF)(&ws_type,&i,err_ind,length_list,
                   &text_facs->font_precs[j].font,
                   &text_facs->font_precs[j].prec,
                   &text_facs->num_char_hts,&minchh,&maxchh,
                   &text_facs->num_char_expans,&minchx,&maxchx,
                   &text_facs->num_pred_inds);
            text_facs->num_font_precs++;
            j++;
        }
    }
}
