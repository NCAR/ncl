/*
 *	$Id: s_ginst.c,v 1.5 2008-07-23 17:24:20 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *  Initialize String
 */

#include <ncarg/gks.h>
#include <string.h>

extern void NGCALLF(ginst,GINST)(Gint*,Gint*,int*,NGstring,Gint*,
                                 const Gfloat*,const Gfloat*,
                                 const Gfloat*,const Gfloat*,
                                 Gint*,Gint*,const size_t*,void*,int);

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
