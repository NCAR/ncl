/*
 *  $Id: s_gqwkc.c,v 1.4 2000-08-22 15:09:12 haley Exp $
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
 *  Inquire workstation connection and type  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwkc,GQWKC)(Gint*,Gint*,int*,Gint*);

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
