/*
 *	$Id: c_aredam.c,v 1.1 1997-04-11 17:40:19 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_aredam
#ifdef NeedFuncProto
(
    int *iam,
    float *xca,
    float *yca,
    int lca,
    int igi,
    int idl,
    int idr
)
#else
(iam,xca,yca,lca,igi,idl,idr)
    int *iam;
    float *xca;
    float *yca;
    int lca;
    int igi;
    int idl;
    int idr;
#endif
{
    NGCALLF(aredam,AREDAM)(iam,xca,yca,&lca,&igi,&idl,&idr);
}
