/*
 *	$Id: c_argtai.c,v 1.1 1997-04-11 17:40:21 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_argtai
#ifdef NeedFuncProto
(
    int *iam,
    float xcd,
    float ycd,
    int *iai,
    int *iag,
    int mai,
    int *nai,
    int icf
)
#else
(iam,xcd,ycd,iai,iag,mai,nai,icf)
    int *iam;
    float xcd;
    float ycd; 
    int *iai;
    int *iag;
    int mai;
    int *nai;
    int icf;
#endif
{
    float xcd2,ycd2;
    xcd2 = xcd;
    ycd2 = ycd;
    NGCALLF(argtai,ARGTAI)(iam,&xcd2,&ycd2,iai,iag,&mai,nai,&icf);
}
