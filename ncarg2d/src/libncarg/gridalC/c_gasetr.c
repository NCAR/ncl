/*
 *	$Id: c_gasetr.c,v 1.1 1997-04-11 17:42:52 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gasetr
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
 *  Make sure parameter name is not NULL
 */
    if( !pnam ) {
        fprintf( stderr, "c_gasetr:  illegal parameter string (NULL)\n");
        return;
    }

    rval2 = rval;
    len = NGSTRLEN(pnam);
    pnam2 = NGCstrToFstr(pnam,len);
    NGCALLF(gasetr,GASETR)(pnam2,&rval2,len);
}
