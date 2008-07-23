/*
 *	$Id: s_ggdp.c,v 1.4 2008-07-23 17:24:20 haley Exp $
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
 *  Generalized drawing primitive
 */

#include <ncarg/gks.h>

void ggdp
#ifdef NeedFuncProto
(
    const Gpoint_list   *point_list, /* list of points  */
    Gint                gdp_id,      /* gdp identifier  */
    const Ggdp_data     *gdp_data    /* gdp data record */
)
#else
( point_list, gdp_id, gdp_data )
    Gpoint_list   *point_list;
    Gint                gdp_id;
    Ggdp_data     *gdp_data;
#endif
{
/*  Note:  This routine does not do anything at this point because
 *         the NCARG GKS package does not use generalized drawing
 *         primitives.  If this changes in the future, then this
 *         routine will be modified accordingly.
 */
}

