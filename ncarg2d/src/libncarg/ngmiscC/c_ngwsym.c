/*
 *	$Id: c_ngwsym.c,v 1.1 1997-04-11 17:43:45 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngwsym
#ifdef NeedFuncProto
(
    char *ftype,
    int num,
    float x,
    float y,
    float size,
    int icolor,
    int ialt
)
#else
(ftype,num,x,y,size,icolor,ialt)
    char *ftype;
    int num;
    float x;
    float y;
    float size;
    int icolor;
    int ialt;
#endif
{
    float x2, y2, size2;
    NGstring ftype2;
    int len;
/*
 * Make sure font parameter is not NULL
 */
    if( !ftype ) {
        fprintf( stderr, "c_ngwsym:  illegal font parameter name (NULL)\n" );
        return;
    }

    x2 = x;
    y2 = y;
    size2 = size;

    len = NGSTRLEN(ftype);
    ftype2 = NGCstrToFstr(ftype,len);
    NGCALLF(ngwsym,NGWSYM)(ftype2,&num,&x2,&y2,&size2,&icolor,&ialt,len);
}
