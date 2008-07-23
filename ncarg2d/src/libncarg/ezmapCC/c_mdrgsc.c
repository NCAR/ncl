/*
 *      $Id: c_mdrgsc.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

#include <ncarg/ncargC.h>

extern void NGCALLF(mdrgsc,MDRGSC)(int[5],int[5]);

void c_mdrgsc
#ifdef NeedFuncProto
(
    int lcol[5],
    int lcsf[5]
)
#else
(idp)
    int lcol[5];
    int lcsf[5];
#endif
{
    NGCALLF(mdrgsc,MDRGSC)(lcol,lcsf);
}
