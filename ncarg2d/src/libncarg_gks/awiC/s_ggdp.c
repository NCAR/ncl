/*
 *	$Id: s_ggdp.c,v 1.3 2000-08-22 15:08:47 haley Exp $
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
 *  Generalized drawing primitive
 */

#include <ncarg/gks.h>

void ggdp
#ifdef NeedFuncProto
(
    const Gpoint_list   *point_list, /* list of points  */
    Gint                gdp_id,      /* gdp identifier  */
    const Ggdp_data     *gdp_data    /* gdp data record */
)
#else
( point_list, gdp_id, gdp_data )
    Gpoint_list   *point_list;
    Gint                gdp_id;
    Ggdp_data     *gdp_data;
#endif
{
/*  Note:  This routine does not do anything at this point because
 *         the NCARG GKS package does not use generalized drawing
 *         primitives.  If this changes in the future, then this
 *         routine will be modified accordingly.
 */
}

