/*
 *	$Id: c_pcmpxy.c,v 1.5 2008-07-23 15:46:49 haley Exp $
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

extern void NGCALLF(pcmpxy,PCMPXY)(int*,float*,float*,float*,float*);

void c_pcmpxy
#ifdef NeedFuncProto
(
    int imap,
    float xinp,
    float yinp,
    float *xotp,
    float *yotp
)
#else
(imap,xinp,yinp,xotp,yotp)
    int imap;
    float xinp;
    float yinp;
    float *xotp;
    float *yotp;
#endif
{
    NGCALLF(pcmpxy,PCMPXY)(&imap,&xinp,&yinp,xotp,yotp);
}
