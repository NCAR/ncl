/*
 *  $Id: setcid.c,v 1.3 2000-08-22 15:09:28 haley Exp $
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
    int i, pos;
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
