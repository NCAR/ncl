/*
 *	$Id: c_wmgtln.c,v 1.1 1997-04-11 17:45:26 haley Exp $
 */
#include <ncarg/ncargC.h>

int c_wmgtln
#ifdef NeedFuncProto
(
    char *lab,
    int lablen,    
    int ilr
)
#else
(lab,lablen,ilr)
    char *lab,
    int lablen,    
    int ilr
#endif
{
	extern int NGCALLF(wmgtln,WMGTLN)();
    NGstring lab2;
    int len;
    len = NGSTRLEN(lab);
    lab2 = NGCstrToFstr(lab,len);
    return(NGCALLF(wmgtln,WMGTLN)(lab2,&lablen,&ilr,len));
}
