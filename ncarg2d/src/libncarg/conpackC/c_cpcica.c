/*
 *	$Id: c_cpcica.c,v 1.1 1997-04-11 17:40:59 haley Exp $
 */
#include <ncarg/ncargC.h>

void c_cpcica
#ifdef NeedFuncProto
(
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *icra,
    int ica1,
    int icam,
    int ican,
    float xcpf,
    float ycpf,
    float xcqf,
    float ycqf
)
#else
(zdat,rwrk,iwrk,icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf)
    float *zdat;
    float *rwrk;
    int *iwrk;
    int *icra;
    int ica1;
    int icam;
    int ican;
    float xcpf;
    float ycpf;
    float xcqf;
    float ycqf;
#endif
{
    float xcpf2, ycpf2, xcqf2, ycqf2;
	int i, j, k, l, icam2, *icra2;
/*
 * Create transpositional array
 */
    icra2 = (int *)malloc(icam*ican*sizeof(int));
    if( icra2 == NULL ) {
        (void)fprintf( stderr, "\nc_cpcica: Unable to create memory for array icra2\n" );
        return;
    }
    icam2 = icam;
    xcpf2 = xcpf;

    ycpf2 = ycpf;
    xcqf2 = xcqf;
    ycqf2 = ycqf;
    NGCALLF(cpcica,CPCICA)(zdat,rwrk,iwrk,icra2,&icam2,&icam,&ican,&xcpf2,&ycpf2,&xcqf2,&ycqf2);
/*
 * Transpose array
 */
    l = 0;
    for( j = 0; j < ican; j++ ) {
        for( i = 0; i < icam; i++ ) {
            k = i * ica1 + j;
            icra[k] = icra2[l++];
        }
    }
    if( icra2 != NULL ) free((int *) icra2);
}
