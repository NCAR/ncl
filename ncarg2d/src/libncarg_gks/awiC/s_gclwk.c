/*
 *	$Id: s_gclwk.c,v 1.1 1997-03-05 19:12:46 haley Exp $
 */
/*
 *  Close workstation  
 */

#include <ncarg/gks.h>

void gclose_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    int err_ind, conn_id, ws_type;
/*
 * Get workstation connection and type associated with the
 * workstation identifier
 */
    NGCALLF(gqwkc,GQWKC)(&ws_id,&err_ind,&conn_id,&ws_type);
/*
 * Remove the connection identifier from list of
 * connection identifiers in use
 */
    remove_conn_id(conn_id);
/*
 * Close workstation
 */
    NGCALLF(gclwk,GCLWK)(&ws_id);
}
