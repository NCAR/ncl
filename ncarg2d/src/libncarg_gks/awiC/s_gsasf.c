/*
 *	$Id: s_gsasf.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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
 * Set aspect source flags 
 */

#include <ncarg/gks.h>

extern void NGCALLF(gsasf,GSASF)(const Gasfs *);

void gset_asfs
#ifdef NeedFuncProto
(
    const Gasfs *list_asf  /* list of aspect source flags */
)
#else
( list_asf )
    Gasfs *list_asf;
#endif
{
    NGCALLF(gsasf,GSASF)(list_asf);
}
