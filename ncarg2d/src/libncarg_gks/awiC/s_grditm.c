/*
 *	$Id: s_grditm.c,v 1.1 1997-03-05 19:13:18 haley Exp $
 */
/*
 *  Read item from GKSM  
 */

#include <ncarg/gks.h>

void gread_item
#ifdef NeedFuncProto
(
    Gint ws_id,                 /* workstation identifier  */
    Gint max_item_data_length,  /* max item data record length */
    Gitem_data *item_data       /* OUT item data record        */
)
#else
( ws_id, max_item_data_length, item_data )
    Gint ws_id;
    Gint max_item_data_length;
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
