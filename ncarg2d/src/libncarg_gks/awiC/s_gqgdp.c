/*
 *  $Id: s_gqgdp.c,v 1.3 2000-08-22 15:08:57 haley Exp $
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
 *  Inquire generalized drawing primitive  
 */

#include <ncarg/gks.h>

void ginq_gdp
#ifdef NeedFuncProto
(
    Gint   ws_type,   /* workstation type            */
    Gint   gdp,       /* GDP function number         */
    Gint   *err_ind,  /* OUT error indicator         */
    Gint   *num_attr, /* OUT num. of attributes used */
    Gattrs attr[4]    /* OUT list of attributes used */
)
#else
( ws_type, gdp, err_ind, num_attr, attr )
    Gint   ws_type;
    Gint   gdp;
    Gint   *err_ind;
    Gint   *num_attr;
    Gattrs attr[4];
#endif
{
/*  Note:  This routine does not do anything at this point because
 *         the NCARG GKS package does not use generalized drawing
 *         primitives.  If this changes in the future, then this
 *         routine will be modified accordingly.
 */
    return;
}
