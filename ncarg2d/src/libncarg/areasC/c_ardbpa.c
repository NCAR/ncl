/*
 *	$Id: c_ardbpa.c,v 1.1 1997-04-11 17:40:12 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ardbpa
#ifdef NeedFuncProto
(
    int *iam,
    int igi,
    char *lab
)
#else
(iam,igi,lab)
    int *iam;
    int igi;
    char *lab;
#endif
{
	NGstring lab2;
    int len;

    len = NGSTRLEN(lab);
	lab2 = NGCstrToFstr(lab,len);
    NGCALLF(ardbpa,ARDBPA)(iam,&igi,lab2,len);
}
