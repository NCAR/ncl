/*
 *	$Id: c_encd.c,v 1.1 1997-04-11 17:44:53 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_encd
#ifdef NeedFuncProto
(
    float valu,
    float ash,
    char *iout,
    int *nc,
    int ioffd
)
#else
(valu,ash,iout,nc,ioffd)
    float valu;
    float ash;
    char *iout;
    int *nc;
    int ioffd;
#endif
{
    float valu2, ash2;
    NGstring iout2;
    int len;
    valu2 = valu;
    ash2 = ash;
    len = NGSTRLEN(iout);
    iout2 = NGCstrToFstr(iout,len);
    NGCALLF(encd,ENCD)(&valu2,&ash2,iout2,nc,&ioffd,len);
}
