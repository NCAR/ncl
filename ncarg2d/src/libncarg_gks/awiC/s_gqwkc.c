/*
 *  $Id: s_gqwkc.c,v 1.1 1997-03-05 19:13:15 haley Exp $
 */
/*
 *  Inquire workstation connection and type  
 */

#include <ncarg/gks.h>

void ginq_ws_conn_type
#ifdef NeedFuncProto
(
    Gint ws_id,         /* workstation identifier                  */
    Gint string_length, /* string length for connection identifier */
    Gint *err_ind,      /* OUT error indicator                     */
    char *conn_id,      /* OUT connection identifier               */
    Gint *ws_type       /* OUT workstation type                    */
)
#else
( ws_id, string_length, err_ind, conn_id, ws_type )
    Gint ws_id;
    Gint string_length;
    Gint *err_ind;
    char *conn_id;
    Gint *ws_type;
#endif
{
    int iconn_id;

    NGCALLF(gqwkc,GQWKC)( &ws_id, err_ind, &iconn_id, ws_type );

    if( conn_id ) {
        sprintf( conn_id, "%d", iconn_id );
    }
}
