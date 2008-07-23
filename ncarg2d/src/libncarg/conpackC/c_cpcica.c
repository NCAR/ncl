/*
 *	$Id: c_cpcica.c,v 1.6 2008-07-23 16:16:42 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdlib.h>

#include <ncarg/ncargC.h>

extern void NGCALLF(cpcica,CPCICA)(float*,float*,int*,int*,int*,int*,int*,
                                   float*,float*,float*,float*);

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

    NGCALLF(cpcica,CPCICA)(zdat,rwrk,iwrk,icra2,&icam2,&icam,&ican,&xcpf,
                           &ycpf,&xcqf,&ycqf);
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
