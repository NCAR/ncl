/*
 *	$Id: s_gcsgwk.c,v 1.1 1997-03-05 19:12:47 haley Exp $
 */
/*
 *  Copy Segment to Workstation
 */

#include <ncarg/gks.h>

void gcopy_seg_ws
#ifdef NeedFuncProto
(
    Gint ws_id,    /* workstation identifier */
    Gint seg_name  /* segment name           */
)
#else
( ws_id, seg_name )
    Gint ws_id;     /* workstation identifier */
    Gint seg_name;  /* segment name           */
#endif
{
    NGCALLF(gcsgwk,GCSGWK)(&ws_id,&seg_name);
}
