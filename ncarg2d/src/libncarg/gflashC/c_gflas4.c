/*
 *	$Id: c_gflas4.c,v 1.1 1997-04-11 17:42:31 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_gflas4
#ifdef NeedFuncProto
(
    int id,
    char *fname
)
#else
(id,fname)
    int id;
    char *fname;
#endif
{
    NGstring fname2;
    int len;
/*
 * Make sure filename is not NULL
 */
    if( !fname ) {
        fprintf( stderr, "c_gflas4:  illegal filename (NULL)\n" );
        return;
    }
    len = NGSTRLEN(fname);
    fname2 = NGCstrToFstr(fname,len);
    NGCALLF(gflas4,GFLAS4)(&id,fname2,len);
}
