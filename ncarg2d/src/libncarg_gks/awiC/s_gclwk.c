/*
 *	$Id: s_gclwk.c,v 1.2 2000-07-12 17:06:07 haley Exp $
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
 *  Close workstation  
 */

#include <ncarg/gks.h>

void gclose_ws
#ifdef NeedFuncProto
(
    Gint ws_id  /* workstation identifier */
)
#else
( ws_id )
    Gint ws_id;
#endif
{
    int err_ind, conn_id, ws_type;
/*
 * Get workstation connection and type associated with the
 * workstation identifier
 */
    NGCALLF(gqwkc,GQWKC)(&ws_id,&err_ind,&conn_id,&ws_type);
/*
 * Remove the connection identifier from list of
 * connection identifiers in use
 */
    remove_conn_id(conn_id);
/*
 * Close workstation
 */
    NGCALLF(gclwk,GCLWK)(&ws_id);
}
