/*
 *	$Id: s_gqpmi.c,v 1.3 2000-08-01 14:35:52 haley Exp $
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
 *  Inquire polymarker index  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqpmi,GQPMI)(Gint*,Gint*);

void ginq_marker_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,    /*  OUT error indicator           */
    Gint *marker_ind  /*  OUT current polymarker index  */
)
#else
( err_ind, marker_ind )
    Gint *err_ind;    /*  OUT error indicator           */
    Gint *marker_ind; /*  OUT current polymarker index  */
#endif
{
    NGCALLF(gqpmi,GQPMI)(err_ind,marker_ind);
}
