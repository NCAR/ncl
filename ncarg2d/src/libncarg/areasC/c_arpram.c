/*
 *	$Id: c_arpram.c,v 1.1 1997-04-11 17:40:23 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_arpram
#ifdef NeedFuncProto
(
    int *iam,
    int if1,
    int if2,
    int if3
)
#else
(iam,if1,if2,if3)
    int *iam;
    int if1;
    int if2;
    int if3;
#endif
{
    NGCALLF(arpram,ARPRAM)(iam,&if1,&if2,&if3);
}
