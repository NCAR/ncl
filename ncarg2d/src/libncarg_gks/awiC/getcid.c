/*
 *  $Id: getcid.c,v 1.2 2000-07-12 17:06:05 haley Exp $
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
