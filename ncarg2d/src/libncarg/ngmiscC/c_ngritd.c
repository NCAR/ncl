/*
 *	$Id: c_ngritd.c,v 1.1 1997-04-11 17:43:42 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_ngritd
#ifdef NeedFuncProto
(
    int iaxs,
    float angl,
    float *ucrd,
    float *vcrd,
    float *wcrd
)
#else
(iaxs,angl,ucrd,vcrd,wcrd)
    int iaxs;
    float angl;
    float *ucrd;
    float *vcrd;
    float *wcrd;
#endif
{
    NGCALLF(ngritd,NGRITD)(&iaxs,&angl,ucrd,vcrd,wcrd);
}
