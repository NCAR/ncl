/*
 *	$Id: s_gqewk.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire list of available workstation types  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqewk,GQEWK)(int*,Gint*,Gint*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_list_avail_ws_types
#ifdef NeedFuncProto
(
    Gint      num_elems_appl_list, /* length of application list     */
    Gint      start_pos,           /* starting position              */
    Gint      *err_ind,            /* OUT error indicator            */
    Gint_list *ws_type,            /* OUT list of available ws types */
    Gint      *length_list         /* OUT length of list in GKS      */
)
#else
( num_elems_appl_list,start_pos,err_ind,ws_type,
                               length_list )
    Gint      num_elems_appl_list;
    Gint      start_pos;
    Gint      *err_ind;
    Gint_list *ws_type;
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
    NGCALLF(gqewk,GQEWK)(&beg_pos,err_ind,length_list,&ws_type->ints[j]);
    ws_type->num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        ws_type->num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqewk,GQEWK)(&i,err_ind,length_list,&ws_type->ints[j]);
            ws_type->num_ints++;
            j++;
        }
    }
}
