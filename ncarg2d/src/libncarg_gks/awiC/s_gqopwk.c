/*
 *	$Id: s_gqopwk.c,v 1.1 1997-03-05 19:13:04 haley Exp $
 */
/*
 *  Inquire set of open workstations
 */

#include <ncarg/gks.h>

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_set_open_wss
#ifdef NeedFuncProto
(
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *open_ws,            /* OUT list of open ws ids    */
    Gint      *length_list         /* OUT length of list in GKS  */
)
#else
(num_elems_appl_list,start_pos,err_ind,open_ws,length_list)
    Gint      num_elems_appl_list;
    Gint      start_pos;
    Gint      *err_ind;
    Gint_list *open_ws;
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
    NGCALLF(gqopwk,GQOPWK)(&beg_pos,err_ind,length_list,&open_ws->ints[j]);
    open_ws->num_ints = *length_list;
/*
 * Get rest of elements
 */
    if( ! *err_ind ) {
        open_ws->num_ints = 1;
        j++;
        num = min( *length_list - beg_pos + 1, num_elems_appl_list );
        end_pos = beg_pos + num - 1;
        for( i = beg_pos + 1; i <= end_pos; i++ ) {
            NGCALLF(gqopwk,GQOPWK)(&i,err_ind,length_list,&open_ws->ints[j]);
            open_ws->num_ints++;
            j++;
        }
    }
}

