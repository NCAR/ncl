/*
 *  $Id: s_gqtxf.c,v 1.5 2008-07-23 17:24:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
