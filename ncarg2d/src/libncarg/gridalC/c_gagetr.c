/*
 *	$Id: c_gagetr.c,v 1.1 1997-04-11 17:42:47 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gagetr
#ifdef NeedFuncProto
(
    char *pnam,
    float *rval
)
#else
(pnam,rval)
    char *pnam;
    float *rval;
#endif
{
    NGstring pnam2;
    int len;
/*
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gagetr:  illegal parameter string (NULL)\n");
        return;
    }
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(gagetr,GAGETR)(pnam2,rval,len);
}
