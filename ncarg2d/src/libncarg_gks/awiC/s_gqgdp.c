/*
 *  $Id: s_gqgdp.c,v 1.1 1997-03-05 19:13:00 haley Exp $
 */
/*
 *  Inquire generalized drawing primitive  
 */

#include <ncarg/gks.h>

void ginq_gdp
#ifdef NeedFuncProto
(
    Gint   ws_type,   /* workstation type            */
    Gint   gdp,       /* GDP function number         */
    Gint   *err_ind,  /* OUT error indicator         */
    Gint   *num_attr, /* OUT num. of attributes used */
    Gattrs attr[4]    /* OUT list of attributes used */
)
#else
( ws_type, gdp, err_ind, num_attr, attr )
    Gint   ws_type;
    Gint   gdp;
    Gint   *err_ind;
    Gint   *num_attr;
    Gattrs attr[4];
#endif
{
/*  Note:  This routine does not do anything at this point because
 *         the NCARG GKS package does not use generalized drawing
 *         primitives.  If this changes in the future, then this
 *         routine will be modified accordingly.
 */
    return;
}
