/*
 *	$Id: s_gsplci.c,v 1.3 2000-08-01 14:35:59 haley Exp $
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
 *  Set polyline colour index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsplci,GSPLCI)(Gint*);

void gset_line_colr_ind
#ifdef NeedFuncProto
(
    Gint line_colr_ind  /* polyline colour index */
)
#else
( line_colr_ind )
    Gint line_colr_ind;
#endif
{
    NGCALLF(gsplci,GSPLCI)(&line_colr_ind);
}
