/*
 *	$Id: s_gqsgus.c,v 1.4 2008-07-23 17:24:23 haley Exp $
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
 *  Inquire set of segment names in use
 */

#include <ncarg/gks.h>

#define min(x,y)    ((x) < (y) ? (x) : (y))

extern void NGCALLF(gqsgus,GQSGUS)(Gint*,Gint*,Gint*,Gint*);

void ginq_set_seg_names
#ifdef NeedFuncProto
(
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *seg_names,          /* OUT list of segment names  */
    Gint      *length_list         /* OUT length of list in GKS  */
)
#else
(num_elems_appl_list,start_pos,err_ind,seg_names,
                       length_list)
    Gint      num_elems_appl_list;
    Gint      start_pos;
    Gint      *err_ind;
    Gint_list *seg_names;
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
    NGCALLF(gqsgus,GQSGUS)(&beg_pos,err_ind,length_list,&seg_names->ints[j]);
    seg_names->num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        seg_names->num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqsgus,GQSGUS)(&i,err_ind,length_list,&seg_names->ints[j]);
            seg_names->num_ints++;
            j++;
        }
    }
}
