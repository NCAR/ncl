/*
 *  $Id: s_gqlwk.c,v 1.1 1997-03-05 19:13:02 haley Exp $
 */
/*
 *  Inquire maximum length of workstation state tables  
 */

#include <ncarg/gks.h>

void ginq_max_ws_st_tables
#ifdef NeedFuncProto
(
    Gint              ws_type,  /* workstation type                  */
    Gint              *err_ind, /* OUT error indicator               */
    Gmax_ws_st_tables *lengths  /* OUT lengths of workstation tables */
)
#else
(ws_type,err_ind,lengths)
    Gint              ws_type;
    Gint              *err_ind;
    Gmax_ws_st_tables *lengths;
#endif
{
    NGCALLF(gqlwk,GQLWK)(&ws_type,err_ind,
                         &lengths->line_bundles,
                         &lengths->marker_bundles,
                         &lengths->text_bundles,
                         &lengths->fill_bundles,
                         &lengths->pat_reps,
                         &lengths->colr_reps);
}
