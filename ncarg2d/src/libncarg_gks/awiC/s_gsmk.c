/*
 *	$Id: s_gsmk.c,v 1.3 2000-08-01 14:35:59 haley Exp $
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
 *  Set marker type  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsmk,GSMK)(Gint*);

void gset_marker_type
#ifdef NeedFuncProto
(
    Gint marker_type  /* marker type  */
)
#else
( marker_type )
    Gint marker_type;
#endif
{
    NGCALLF(gsmk,GSMK)(&marker_type);
}
