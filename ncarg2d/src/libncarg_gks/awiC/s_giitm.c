/*
 *	$Id: s_giitm.c,v 1.1 1997-03-05 19:12:50 haley Exp $
 */
/*
 *  Interpret  
 */

#include <ncarg/gks.h>

void ginterpret_item
#ifdef NeedFuncProto
(
    Gint type,                    /* item type               */
    Gint item_data_length,        /* item data record length */
    const Gitem_data *item_data   /* item data record        */
)
#else
( type, item_data_length, item_data )
    Gint type;
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
