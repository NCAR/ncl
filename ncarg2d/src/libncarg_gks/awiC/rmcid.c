/*
 *  $Id: rmcid.c,v 1.2 2000-07-12 17:06:06 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
