/*
 *  $Id: s_gqwks.c,v 1.4 2000-08-22 15:09:13 haley Exp $
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
 *  Inquire workstation state  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqwks,GQWKS)(Gint*,Gint*,Gws_st*);

void ginq_ws_st
#ifdef NeedFuncProto
(
    Gint   ws_id,    /* workstation identifier */
    Gint   *err_ind, /* OUT error indicator    */
    Gws_st *ws_st    /* OUT workstation state  */
)
#else
( ws_id, err_ind, ws_st )
    Gint   ws_id;
    Gint   *err_ind;
    Gws_st *ws_st;
#endif
{
    NGCALLF(gqwks,GQWKS)(&ws_id,err_ind,ws_st);
}
