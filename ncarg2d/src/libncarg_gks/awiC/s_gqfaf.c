/*
 *  $Id: s_gqfaf.c,v 1.6 2008-07-23 17:24:21 haley Exp $
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

/*
 *  Inquire fill area facilities  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gqfaf,GQFAF)(Gint*,int*,Gint*,Gint*,Gint*,
                                 Gfill_int_style*,Gint*,Gint*,Gint*);

#define min(x,y)   ((x) < (y) ? (x) : (y))

void ginq_fill_facs
#ifdef NeedFuncProto
(
    Gint       ws_type,           /* workstation type                */
    Gint       hatch_length,      /* length of hatch style list      */
    Gint       hatch_start_pos,   /* hatch style starting position   */
    Gint       *err_ind,          /* OUT error indicator             */
    Gfill_facs *fill_facs,        /* OUT fill area facilities        */
    Gint       *act_hatch_length  /* OUT length of hatch list in GKS */
)
#else
(ws_type,hatch_length,hatch_start_pos,err_ind,fill_facs,
                    act_hatch_length)
    Gint       ws_type;
    Gint       hatch_length;
    Gint       hatch_start_pos;
    Gint       *err_ind;
    Gfill_facs *fill_facs;
    Gint       *act_hatch_length;
#endif
{
    int num, ni = 0, i, nhs = 0;
    int idum1, idum2, hatch_end_pos;
    Gfill_int_style idum3;

    NGCALLF(gqfaf,GQFAF)(&ws_type,&ni,&hatch_start_pos,err_ind,
                         &fill_facs->num_int_styles,
                         &fill_facs->int_style[ni],
                         act_hatch_length,
                         &fill_facs->hatch_styles.ints[nhs],
                         &fill_facs->num_pred_inds);
    if( ! *err_ind ) {
        for( ni = 1; ni < fill_facs->num_int_styles; ni++ ) {
            NGCALLF(gqfaf,GQFAF)(&ws_type,&ni,&idum1,err_ind,
                                 &fill_facs->num_int_styles,
                                 &fill_facs->int_style[ni],
                                 act_hatch_length,&idum2,
                                 &fill_facs->num_pred_inds);
        }
        num = min(hatch_length, *act_hatch_length - hatch_start_pos + 1);
        hatch_end_pos = hatch_start_pos + num - 1;
        nhs++;
        for( i = hatch_start_pos + 1; i <= hatch_end_pos; i++ ) {
            NGCALLF(gqfaf,GQFAF)(&ws_type,&idum1,&i,err_ind,
                                 &fill_facs->num_int_styles,&idum3,
                                 act_hatch_length,
                                 &fill_facs->hatch_styles.ints[nhs++],
                                 &fill_facs->num_pred_inds);
        }
        fill_facs->hatch_styles.num_ints = nhs;
    }
}
