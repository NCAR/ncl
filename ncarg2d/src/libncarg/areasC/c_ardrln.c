/*
 *	$Id: c_ardrln.c,v 1.1 1997-04-11 17:40:17 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ardrln
#ifdef NeedFuncProto
(
    int *iam,
    float *xcd,
    float *ycd,
    int ncd,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*colrln_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
               )
)
#else
(iam,xcd,ycd,ncd,xcs,ycs,mcs,iai,iag,mai,colrln_)
    int *iam;
    float *xcd;
    float *ycd;
    int ncd;
    float *xcs;
    float *ycs;
    int mcs;
    int *iai;
    int *iag;
    int mai;
    int (*colrln_)();
#endif
{
    NGCALLF(ardrln,ARDRLN)(iam,xcd,ycd,&ncd,xcs,ycs,&mcs,iai,iag,&mai,colrln_);
}
