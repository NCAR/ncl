/*
 *	$Id: s_ggtitm.c,v 1.1 1997-03-05 19:12:50 haley Exp $
 */
/*
 *  Get item type from gksm  
 */

#include <ncarg/gks.h>

void gget_item_type
#ifdef NeedFuncProto
(
    Gint ws_id,              /* workstation identifier      */
    Gint *item_type,         /* OUT item type               */
    Gint *item_data_length   /* OUT item data record length */
)
#else
( ws_id, item_type, item_data_length )
    Gint ws_id;
    Gint *item_type;
    Gint *item_data_length;
#endif
{
    NGCALLF(ggtitm,GGTITM)(&ws_id,item_type,item_data_length);
}
