/*
 *  $Id: setcid.c,v 1.4 2008-07-23 17:24:25 haley Exp $
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
 * Set the smallest available connection identifier
 */

#include <stdio.h>

#define MAX_OPEN_WK  15

extern int conn_id_list[];
extern int smallest_avail_id;
extern int num_used_conn_ids;

void set_avail_conn_id
#ifdef NeedFuncProto
(
    void
)
#else
()
#endif
{
  int i;
/*
 * Get the smallest available connection identifier.
 * Cannot use either "5" or "6" since these are stdin and stdout
 * respectively.
 */
    smallest_avail_id = 0;
    for( i = 0; i < num_used_conn_ids - 1; i++ ) {
        if( conn_id_list[i] != 4 && conn_id_list[i] != conn_id_list[i+1] ) {
            smallest_avail_id = conn_id_list[i] + 1;
            break;
        }
        else if( conn_id_list[i] == 4 && conn_id_list[i+1] != 7 ) {
            smallest_avail_id = 7;
        }
    }
    if( !smallest_avail_id ) smallest_avail_id = conn_id_list[num_used_conn_ids-1] + 1;           
}
