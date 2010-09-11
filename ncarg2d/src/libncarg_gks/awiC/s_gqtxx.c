/*
 *	$Id: s_gqtxx.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 * Inquire text extent
 */

#include <ncarg/gks.h>
#include <string.h>

extern void NGCALLF(gqtxx,GQTXX)(Gint*,const Gfloat*,const Gfloat*,NGstring,
                                 Gint*,Gfloat*,Gfloat*,Gfloat x[4],
                                 Gfloat y[4],int);

void ginq_text_extent
#ifdef NeedFuncProto
(
    Gint         ws_id,    /* workstation identifier      */
    const Gpoint *pos,     /* text position               */
    const char   *str,     /* text string                 */
    Gint         *err_ind, /* OUT error indicator         */
    Gtext_extent *extent   /* OUT concatentation point and
							  text extent parallelogram   */
)
#else
( ws_id, pos, str, err_ind, extent )
    Gint         ws_id;
    Gpoint       *pos;
    char         *str;
    Gint         *err_ind;
    Gtext_extent *extent;
#endif
{
    int i, len;
    Gfloat x[4], y[4];
    NGstring str2;

    len = NGSTRLEN(str);
    str2 = NGCstrToFstr(str,len);
    NGCALLF(gqtxx,GQTXX)(&ws_id,&pos->x,&pos->y,str2,err_ind,
                         &extent->concat_point.x,&extent->concat_point.y,
                         x,y,len);
    for( i = 0; i < 4; i++ ) {
        extent->paral[i].x = (Gfloat) x[i];
        extent->paral[i].y = (Gfloat) y[i];
    }
}
