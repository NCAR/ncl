/*
 *  $Id: addcid.c,v 1.3 2000-08-22 15:08:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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
