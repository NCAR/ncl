/*
 *	$Id: s_ggtitm.c,v 1.4 2008-07-23 17:24:20 haley Exp $
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
 *  Get item type from gksm  
 */

#include <ncarg/gks.h>

extern void NGCALLF(ggtitm,GGTITM)(Gint*,Gint*,Gint*);

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
