/*
 *	$Id: s_gtx.c,v 1.1 1997-03-05 19:13:33 haley Exp $
 */
/*
 *  Text  
 */

#include <ncarg/gks.h>

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
