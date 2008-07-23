/*
 *  $Id: s_gqplf.c,v 1.5 2008-07-23 17:24:22 haley Exp $
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
 *  Inquire polyline facilities  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqplf,GQPLF)(Gint*,int*,Gint*,Gint*,Gint*,Gint*,
                                 Gfloat*,Gfloat*,Gfloat*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_line_facs
#ifdef NeedFuncProto
(
    Gint       ws_type,             /* workstation type                   */
    Gint       num_elems_appl_list, /* length of application list         */
    Gint       start_pos,           /* starting position                  */
    Gint       *err_ind,            /* OUT error indicator                */
    Gline_facs *line_facs,          /* OUT polyline facilities            */
    Gint       *length_list         /* OUT length of linetype list in GKS */
)
#else
(ws_type,num_elems_appl_list,start_pos,err_ind,
                    line_facs,length_list)
    Gint       ws_type;
    Gint       num_elems_appl_list;
    Gint       start_pos;
    Gint       *err_ind;
    Gline_facs *line_facs;
    Gint       *length_list;
#endif
{
    int num, i, j = 0, beg_pos, end_pos;
    Gfloat nomlw, rlwmin, rlwmax;
/*
 * Position "n" in C is position "n+1" in Fortran
 */
    beg_pos = start_pos + 1;
/*
 * Get first element
 */
    NGCALLF(gqplf,GQPLF)(&ws_type,&beg_pos,err_ind,length_list,
                         &line_facs->types.ints[j],
                         &line_facs->num_widths,
                         &nomlw,&rlwmin,&rlwmax,
                         &line_facs->num_pred_inds);
    line_facs->nom_width = (Gdouble)nomlw;
    line_facs->min_width = (Gdouble)rlwmin;
    line_facs->max_width = (Gdouble)rlwmax;
    line_facs->types.num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        line_facs->types.num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqplf,GQPLF)(&ws_type,&i,err_ind,length_list,
                                 &line_facs->types.ints[j],
                                 &line_facs->num_widths,
                                 &nomlw,&rlwmin,&rlwmax,
                                 &line_facs->num_pred_inds);
            line_facs->types.num_ints++;
            j++;
        }
    }
}
