/*
 *	$Id: s_gqops.c,v 1.1 1997-03-05 19:13:03 haley Exp $
 */
/*
 *  Inquire operating state value  
 */

#include <ncarg/gks.h>

void ginq_op_st
#ifdef NeedFuncProto
(
    Gop_st *op_st  /* OUT operating state value */
)
#else
( op_st )
    Gop_st *op_st;
#endif
{
    NGCALLF(gqops,GQOPS)( op_st );
}
