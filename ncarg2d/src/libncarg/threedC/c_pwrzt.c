/*
 *	$Id: c_pwrzt.c,v 1.1 1997-04-11 17:45:10 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_pwrzt 
#ifdef NeedFuncProto
(
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
)
#else
 (x,y,z,id,n,isize,lin3,itop,icnt)
    float x;
    float y;
    float z;
    char *id;
    int n;
    int isize;
    int lin3;
    int itop;
    int icnt;
#endif
{
    float x2, y2, z2;
	NGstring id2;
    int len;
    x2 = x;
    y2 = y;
    z2 = z;
    len = NGSTRLEN(id);
	id2 = NGCstrToFstr(id,len);
    NGCALLF(pwrzt,PWRZT)(&x2,&y2,&z2,id2,&n,&isize,&lin3,&itop,&icnt,len);
}
