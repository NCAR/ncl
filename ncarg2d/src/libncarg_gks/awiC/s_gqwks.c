/*
 *  $Id: s_gqwks.c,v 1.1 1997-03-05 19:13:17 haley Exp $
 */
/*
 *  Inquire workstation state  
 */

#include <ncarg/gks.h>

void ginq_ws_st
#ifdef NeedFuncProto
(
    Gint   ws_id,    /* workstation identifier */
    Gint   *err_ind, /* OUT error indicator    */
    Gws_st *ws_st    /* OUT workstation state  */
)
#else
( ws_id, err_ind, ws_st )
    Gint   ws_id;
    Gint   *err_ind;
    Gws_st *ws_st;
#endif
{
    NGCALLF(gqwks,GQWKS)(&ws_id,err_ind,ws_st);
}
