/*
 *	$Id: s_gqops.c,v 1.5 2008-07-23 17:24:21 haley Exp $
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
 *  Inquire operating state value  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqops,GQOPS)(Gop_st*);

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
