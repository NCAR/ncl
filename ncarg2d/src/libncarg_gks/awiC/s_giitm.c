/*
 *	$Id: s_giitm.c,v 1.4 2008-07-23 17:24:20 haley Exp $
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
