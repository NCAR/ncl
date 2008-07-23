/*
 *  $Id: rmcid.c,v 1.4 2008-07-23 17:24:19 haley Exp $
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
 * Remove connection identifier from list
 */

#include <stdio.h>

extern int conn_id_list[];
extern int smallest_avail_id;
extern int num_used_conn_ids;

void remove_conn_id
#ifdef NeedFuncProto
(
     int conn_id
)
#else
( conn_id )
    int conn_id;
#endif
{
    int i, pos, found = 0;
/*
 * Search for conn_id in list and tag its position
 */
    for( i = 0; i < num_used_conn_ids; i++ ) {
        if( conn_id_list[i] == conn_id ) {
            pos = i;
            found = 1;
            break;
        }
    }
    if( found ) {
        for( i = pos; i < num_used_conn_ids - 1; i++ ) {
            conn_id_list[i] = conn_id_list[i+1];
        }
        conn_id_list[num_used_conn_ids-1] = 0;
        num_used_conn_ids--;
/*
 *  Set new available connection ID if one you just deleted is
 *  a smaller number
 */
        if( conn_id < smallest_avail_id ) smallest_avail_id = conn_id;
    }
}
