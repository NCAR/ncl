/*
 *	$Id: s_ggdp.c,v 1.1 1997-03-05 19:12:50 haley Exp $
 */
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

