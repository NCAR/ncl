/*
 *  $Id: getcid.c,v 1.4 2008-07-23 17:24:19 haley Exp $
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
 * Get smallest available connection identifier
 */

#include <stdio.h>

#define MAX_OPEN_WK  15

int conn_id_list[MAX_OPEN_WK+1];
int smallest_avail_id = 2;
int num_used_conn_ids = 0;

extern void add_conn_id(int*);
extern void set_avail_conn_id();

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
