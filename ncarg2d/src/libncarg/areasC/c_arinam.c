/*
 *	$Id: c_arinam.c,v 1.1 1997-04-11 17:40:22 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_arinam
#ifdef NeedFuncProto
(
    int *iam,
    int lam
)
#else
(iam,lam)
    int *iam;
    int lam;
#endif
{
    NGCALLF(arinam,ARINAM)(iam,&lam);
}
