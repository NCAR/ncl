/*
 *  $Id: getcid.c,v 1.1 1997-03-05 19:12:43 haley Exp $
 */
/*
 * Get smallest available connection identifier
 */

#include <stdio.h>

#define MAX_OPEN_WK  15

int conn_id_list[MAX_OPEN_WK+1];
int smallest_avail_id = 2;
int num_used_conn_ids = 0;

void get_conn_id
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
 * Get the available connection id
 */
    *conn_id = smallest_avail_id;

    if( num_used_conn_ids < MAX_OPEN_WK ) {
/*
 * Add this conn_id to list
 */
        add_conn_id( conn_id );
/*
 * Set the next smallest available connection identifier.
 */
        set_avail_conn_id();
    }
}
