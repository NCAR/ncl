/*
 *      $Id: c_mdrgol.c,v 1.4 2008-07-23 16:16:51 haley Exp $
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

extern void NGCALLF(mdrgol,MDRGOL)(int*,float*,int*);

void c_mdrgol
#ifdef NeedFuncProto
(
    int irgl,
    float *rwrk,
    int lrwk
)
#else
(irgl,rwrk,lrwk)
    int irgl;
    float *rwrk;
    int lrwk;
#endif
{
    NGCALLF(mdrgol,MDRGOL)(&irgl,rwrk,&lrwk);
}
