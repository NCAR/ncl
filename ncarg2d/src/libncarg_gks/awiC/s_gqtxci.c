/*
 *	$Id: s_gqtxci.c,v 1.2 2000-07-12 17:06:18 haley Exp $
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
 *  Inquire text colour index  
 */

#include <ncarg/gks.h>

void ginq_text_colr_ind
#ifdef NeedFuncProto
(
    Gint *err_ind,        /* OUT error indicator     */
    Gint *text_colr_ind   /* OUT text colour index   */
)
#else
( err_ind, text_colr_ind )
    Gint *err_ind;
    Gint *text_colr_ind;
#endif
{
    NGCALLF(gqtxci,GQTXCI)(err_ind,text_colr_ind);
}
