/*
 *  $Id: addcid.c,v 1.1 1997-03-05 19:12:43 haley Exp $
 */
/*
 * Add connection identifier to list
 */

#include <stdio.h>

extern int conn_id_list[];
extern int smallest_avail_id;
extern int num_used_conn_ids;

void add_conn_id
#ifdef NeedFuncProto
(
     int *conn_id
)
#else
( conn_id )
    int *conn_id;
#endif
{
    int i, pos;

/*
 * Insert this new connection id into list (in numerical order)
 */
    pos = num_used_conn_ids;
    for( i = 0; i < num_used_conn_ids; i++ ) {
        if( *conn_id < conn_id_list[i] ) {
            pos = i;
            break;
        }
    }
    for( i = num_used_conn_ids - 1; i >= pos; i-- ) {
        conn_id_list[i+1] = conn_id_list[i];
    }
    conn_id_list[pos] = *conn_id;
    num_used_conn_ids++;
}
