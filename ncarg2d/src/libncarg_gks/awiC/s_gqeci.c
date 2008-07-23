/*
 *	$Id: s_gqeci.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire list of colour indices
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqeci,GQECI)(Gint*,int*,Gint*,Gint*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_list_colr_inds
#ifdef NeedFuncProto
(
    Gint      ws_id,              /* workstation identifier             */
    Gint      num_elems_appl_list,/* length of application list         */
    Gint      start_pos,          /* starting position                  */
    Gint      *err_ind,           /* OUT error indicator                */
    Gint_list *def_colr_inds,     /* OUT list of defined colour indices */
    Gint      *length_list        /* OUT length of list in GKS          */
)
#else
(ws_id,num_elems_appl_list,start_pos,err_ind,def_colr_inds,length_list)
    Gint      ws_id;
    Gint      num_elems_appl_list;
    Gint      start_pos;
    Gint      *err_ind;
    Gint_list *def_colr_inds;
    Gint      *length_list;
#endif
{
    int num, i, j = 0, beg_pos, end_pos;
/*
 * Position "n" in C is position "n+1" in Fortran
 */
    beg_pos = start_pos + 1;
/*
 * Get first element
 */
    NGCALLF(gqeci,GQECI)(&ws_id,&beg_pos,err_ind,length_list,
                         &def_colr_inds->ints[j]);
    def_colr_inds->num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        def_colr_inds->num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqeci,GQECI)(&ws_id,&i,err_ind,length_list,
                                 &def_colr_inds->ints[j]);
            def_colr_inds->num_ints++;
            j++;
        }
    }
}
