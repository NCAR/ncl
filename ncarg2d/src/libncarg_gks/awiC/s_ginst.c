/*
 *	$Id: s_ginst.c,v 1.2 2000-07-12 17:06:09 haley Exp $
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
 *  Initialize String
 */

#include <ncarg/gks.h>

void ginit_string
#ifdef NeedFuncProto
(
    Gint               ws_id,        /* workstation identifier  */
    Gint               string_num,   /* string device number    */
    const char         *init_string, /* initial string          */
    Gint               pet,          /* prompt and echo type    */
    const Glimit       *echo_area,   /* echo area               */
    Gint               in_buf_size,  /* input buffer size       */
    Gint               init_cur_pos, /* initial cursor position */
    const Gstring_data *string_data  /* string data record      */
)
#else
(ws_id,string_num,init_string,pet,echo_area,in_buf_size,
                 init_cur_pos,string_data)
    Gint         ws_id;
    Gint         string_num;
    char         *init_string;
    Gint         pet;
    Glimit       *echo_area;
    Gint         in_buf_size;
    Gint         init_cur_pos;
    Gstring_data *string_data;
#endif
{
    int idum = 0, len;
    NGstring str2;
    len = NGSTRLEN(init_string);
    str2 = NGCstrToFstr(init_string,len);
    NGCALLF(ginst,GINST)(&ws_id,&string_num,&idum,str2,&pet,
                         &echo_area->x_min,&echo_area->x_max,
                         &echo_area->y_min,&echo_area->y_max,
                         &in_buf_size,&init_cur_pos,
                         &string_data->pet_r1.size,
                         string_data->pet_r1.data,len);
}
