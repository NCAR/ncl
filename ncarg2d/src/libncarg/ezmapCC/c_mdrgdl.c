/*
 *      $Id: c_mdrgdl.c,v 1.2 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdrgdl,MDRGDL)(int*);

void c_mdrgdl
#ifdef NeedFuncProto
(
    int *irgl
)
#else
(irgl)
    int *irgl;
#endif
{
    NGCALLF(mdrgdl,MDRGDL)(irgl);
}
