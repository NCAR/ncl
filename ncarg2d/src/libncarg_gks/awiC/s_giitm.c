/*
 *	$Id: s_giitm.c,v 1.2 2000-07-12 17:06:09 haley Exp $
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
 *  Interpret  
 */

#include <ncarg/gks.h>

void ginterpret_item
#ifdef NeedFuncProto
(
    Gint type,                    /* item type               */
    Gint item_data_length,        /* item data record length */
    const Gitem_data *item_data   /* item data record        */
)
#else
( type, item_data_length, item_data )
    Gint type;
    Gint item_data_length;
    Gitem_data *item_data;
#endif
{
/*  Note:  This routine does not do anything at this point because
 *         the NCARG GKS package does not do anything with data items.
 *         If this changes in the future, then this routine will be 
 *         modified accordingly.
 */
    return;
}
