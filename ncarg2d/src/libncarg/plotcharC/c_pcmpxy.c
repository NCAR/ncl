/*
 *	$Id: c_pcmpxy.c,v 1.1 1997-04-11 17:43:51 haley Exp $
 */
#include <ncarg/ncargC.h>

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
    float xinp2, yinp2;
    xinp2 = xinp;
    yinp2 = yinp;
    NGCALLF(pcmpxy,PCMPXY)(&imap,&xinp2,&yinp2,xotp,yotp);
}
