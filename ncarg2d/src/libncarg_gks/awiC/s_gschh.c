/*
 *	$Id: s_gschh.c,v 1.2 2000-07-12 17:06:20 haley Exp $
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
 *  Set character height  
 */

#include <ncarg/gks.h>

void gset_char_ht
#ifdef NeedFuncProto
(
    Gdouble char_ht  /* character height */
)
#else
( char_ht )
    Gdouble char_ht;
#endif
{
    Gfloat ht;
    ht = (Gfloat) char_ht;
    NGCALLF(gschh,GSCHH)(&ht);
}
