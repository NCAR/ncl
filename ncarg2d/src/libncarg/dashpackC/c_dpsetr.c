/*
 *	$Id: c_dpsetr.c,v 1.1 1997-04-11 17:41:46 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_dpsetr
#ifdef NeedFuncProto
(
    char *pnam,
    float rval
)
#else
(pnam,rval)
    char *pnam;
    float rval;
#endif
{
    float rval2;
	NGstring pnam2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !pnam ) { 
        fprintf( stderr, "c_dpsetr:  illegal parameter string (NULL)\n" );
        return;
    }

    rval2 = rval;

    len = NGSTRLEN(pnam);
	pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(dpsetr,DPSETR)(pnam2,&rval2,len);
}
