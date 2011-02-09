/*
 *	$Id: s_gtx.c,v 1.5 2008-07-23 17:24:25 haley Exp $
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
 *  Text  
 */

#include <ncarg/gks.h>
#include <string.h>

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
