/*
 *	$Id: c_cpmviw.c,v 1.1 1997-04-11 17:41:08 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpmviw
#ifdef NeedFuncProto
(
    int *iwko,
    int *iwrk,
    int lwkn
)
#else
(iwko,iwrk,lwkn)
    int *iwko;
    int *iwrk;
    int lwkn;
#endif
{
    NGCALLF(cpmviw,CPMVIW)(iwko,iwrk,&lwkn);
}
