/*
 *	$Id: c_ispltf.c,v 1.1 1997-04-11 17:43:22 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ispltf
#ifdef NeedFuncProto
(
    float rxn,
    float ryn,
    int ient
)
#else
(rxn,ryn,ient)
    float rxn;
    float ryn;
    int ient;
#endif
{
    float rxn2, ryn2;
    
    rxn2 = rxn;
    ryn2 = ryn;
    NGCALLF(ispltf,ISPLTF)(&rxn2,&ryn2,&ient);
}
