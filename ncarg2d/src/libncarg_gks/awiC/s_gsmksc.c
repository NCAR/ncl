/*
 *	$Id: s_gsmksc.c,v 1.3 2000-08-01 14:35:59 haley Exp $
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
 *  Set marker size scale factor  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsmksc,GSMKSC)(Gfloat*);

void gset_marker_size
#ifdef NeedFuncProto
(
    Gdouble marker_size  /* marker size scale factor */
)
#else
( marker_size )
    Gdouble marker_size;
#endif
{
    Gfloat size;
    size = (Gfloat)marker_size;
    NGCALLF(gsmksc,GSMKSC)(&size);
}
