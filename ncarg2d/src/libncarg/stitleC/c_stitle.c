/*
 *	$Id: c_stitle.c,v 1.1 1997-04-11 17:44:46 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_stitle
#ifdef NeedFuncProto
(
    char *crds[],
    int ncds,
    int iyst,
    int iynd,
    float tmst,
    float tmmv,
    float tmnd,
    int mtst
)
#else
(crds,ncds,iyst,iynd,tmst,tmmv,tmnd,mtst)
    char *crds[];
    int ncds;
    int iyst;
    int iynd;
    float tmst;
    float tmmv;
    float tmnd;
    int mtst;
#endif
{
    float tmst2,tmmv2,tmnd2;
    int i, len, maxlen;
    char *crds2;
    NGstring crds22;

    maxlen = strlen(crds[0]);
    for( i = 1; i < ncds; i++ ) {
	len = strlen(crds[i]);
        maxlen = len > maxlen ? len : maxlen;
    }
    crds2 = (char *)malloc((maxlen*ncds+1)*sizeof(char));
    if( crds2 == NULL ) {
	printf( "\nc_stitle:  Unable to create memory for array crds2\n" );
        return;
    }
    Pad_char_array(crds,crds2,ncds,maxlen);
    tmst2 = tmst;
    tmmv2 = tmmv;
    tmnd2 = tmnd;
    crds22 = NGCstrToFstr(crds2,maxlen);
    NGCALLF(stitle,STITLE)(crds22,&ncds,&iyst,&iynd,&tmst2,&tmmv2,&tmnd2,
			   &mtst,maxlen);
    free((char *) crds2);
}
