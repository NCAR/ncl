/*
 *	$Id: s_gtx.c,v 1.4 2000-08-22 15:09:27 haley Exp $
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
 *  Text  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gtx,GTX)(const Gfloat*,const Gfloat*,NGstring,int);

void gtext
#ifdef NeedFuncProto
(
    const Gpoint *text_pos,    /* text position    */
    const char   *char_string  /* character string */
)
#else
(text_pos, char_string )
    Gpoint *text_pos;
    char   *char_string;
#endif
{
    NGstring chars2;
    int len;
    len = NGSTRLEN(char_string);
    chars2 = NGCstrToFstr(char_string,len);
    NGCALLF(gtx,GTX)(&text_pos->x,&text_pos->y,chars2,len);
}
