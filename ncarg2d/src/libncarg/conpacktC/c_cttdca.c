/*
 *      $Id: c_cttdca.c,v 1.1 2004-03-19 23:01:01 kennison Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <stdlib.h>

#include <ncarg/ncargC.h>

extern void NGCALLF(cttdca,CTTDCA)(float*,int*,int*,float*,int*,
                                   int*,int*,int*,int*,
                                   float*,float*,float*,float*);

void c_cttdca
#ifdef NeedFuncProto
(
    float *rpnt,
    int *iedg,
    int *itri,
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
(rpnt,iedg,itri,rwrk,iwrk,icra,ica1,icam,ican,xcpf,ycpf,xcqf,ycqf)
    float *rpnt;
    int *iedg;
    int *itri;
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
        (void)fprintf( stderr, "\nc_ctcica: Unable to create memory for array icra2\n" );
        return;
    }
    icam2 = icam;

    NGCALLF(cttdca,CTTDCA)(rpnt,iedg,itri,rwrk,iwrk,
                           icra2,&icam2,&icam,&ican,
                           &xcpf,&ycpf,&xcqf,&ycqf);
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
