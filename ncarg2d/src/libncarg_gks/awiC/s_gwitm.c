/*
 *	$Id: s_gwitm.c,v 1.1 1997-03-05 19:13:35 haley Exp $
 */
/*
 *  Write item to GKSM
 */

#include <ncarg/gks.h>

void gwrite_item
#ifdef NeedFuncProto
(
    Gint ws_id,                   /* workstation identifier  */
    Gint item_type,               /* item type               */
    Gint item_data_length,        /* item data record length */
    const Gitem_data *item_data   /* item data record        */
)
#else
( ws_id, item_type, item_data_length, item_data )
    Gint ws_id;
    Gint item_type;
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

