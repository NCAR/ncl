/*
 *  $Id: s_gqegdp.c,v 1.2 2000-07-12 17:06:12 haley Exp $
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
 *  Inquire list of available generalized drawing primitives  
 */

#include <ncarg/gks.h>

void ginq_list_avail_gdps
#ifdef NeedFuncProto
(
    Gint      ws_type,             /* workstation identifier     */
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *gdp,                /* OUT list of GDPs           */
    Gint      *length_list         /* OUT length of list in GKS  */
)
#else
(ws_type,num_elems_appl_list,start_pos,err_ind,gdp,length_list)
    Gint      ws_type;
    Gint      num_elems_appl_list;
    Gint      start_pos;
    Gint      *err_ind;
    Gint_list *gdp;
    Gint      *length_list;
#endif
{
/*  Note:  This routine does not do anything at this point because
 *         the NCARG GKS package does not use generalized drawing
 *         primitives.  If this changes in the future, then this
 *         routine will be modified accordingly.
 */
}
