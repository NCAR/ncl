/*
 *	$Id: s_gqopsg.c,v 1.2 2000-07-12 17:06:14 haley Exp $
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
 *  Inquire name of open segment
 */

#include <ncarg/gks.h>

void ginq_name_open_seg
#ifdef NeedFuncProto
(
    Gint *err_ind,      /* OUT error indicator      */
    Gint *name_open_seg /* OUT name of open segment */
)
#else
( err_ind, name_open_seg )
    Gint *err_ind;
    Gint *name_open_seg;
#endif
{
    NGCALLF(gqopsg,GQOPSG)(err_ind,name_open_seg);
}
