/*
 *	$Id: c_armvam.c,v 1.1 1997-04-11 17:40:23 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_armvam
#ifdef NeedFuncProto
(
    int *iam,
    int *ian,
    int lan
)
#else
(iam,ian,lan)
    int *iam;
    int *ian;
    int lan;
#endif
{
    NGCALLF(armvam,ARMVAM)(iam,ian,&lan);
}
