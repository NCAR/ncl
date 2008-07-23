/*
 *      $Id: pdf_conv.c,v 1.2 2008-07-23 17:28:00 haley Exp $
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
 *      File:           pdf_conv.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Tue Jul 27 11:50:09 MDT 1993
 *
 *      Description:    These are the PostScript conversion routines 
 *                      that convert raw data to their device dependent 
 *                      format. These routines assume sufficient space 
 *                      exists in the destination list. 
 */

#include <stdio.h>
#include "gksc.h"
#include "pdf.h"
#include "pdfddi.h"
#include "pdf_device.h"


/*ARGSUSED*/
void    PDFConvPoints(GKSC_Ptr ddp, float *rawx, float *rawy, 
                      Points *points, int *n, int conv)
{
  PDFddp   *psa = (PDFddp *) ddp;
  PDFPoint *pdf_point_ptr = (PDFPoint *) points->list; 
  int      i, index = points->num;

  if (conv == RAW_TO_COOKED) {
    for (i=0; i<*n; index++, i++) {
      pdf_point_ptr[index].x = (((psa->transform).x_scale * rawx[i]) +
                                (psa->transform).x_trans);
      pdf_point_ptr[index].y = (((psa->transform).y_scale * rawy[i]) +
                                (psa->transform).y_trans);
      }
      points->num = index;
    }
    else {
      for (i=0; i<*n; i++) {
        rawx[i] = (pdf_point_ptr[i].x-(psa->transform).x_trans) /
                                   (psa->transform).x_scale;
        rawy[i] = (pdf_point_ptr[i].y-(psa->transform).y_trans) /
                                   (psa->transform).y_scale;
      }
   }
}

/*ARGSUSED*/
void    PDFConvString(ddp, raw, string, n, conv)
        GKSC_Ptr        ddp;
        int     *raw;
        String  *string;
        int     *n;
        int     conv;
{
        char    *s = (char *) string->list;

        int     index = string->num;

        int     i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; i++, index++) {
                        s[index] = raw[i];
                }

                s[index] = '\0';
                string->num = index;
        }
        else {
                for (i=0; i<*n; i++) {
                        raw[i] = s[i];
                }
        }
}

/*ARGSUSED*/
void    PDFConvInts(ddp, raw, ints, n, conv)
        GKSC_Ptr        ddp;
        int     *raw;
        Ints    *ints;
        int     *n;
        int     conv;
{
        int     *iptr = (int *) ints->list;
        int     index = ints->num;

        int     i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; i++, index++) {
                        iptr[index] = raw[i];
                }

                ints->num = index;
        }
        else {
                for (i=0; i<*n; i++) {
                        raw[i] = iptr[i];
                }
        }
}

/*ARGSUSED*/
void    PDFConvFloats(ddp, raw, floats, n, conv)
        GKSC_Ptr        ddp;
        float   *raw;
        Floats  *floats;
        int     *n;
        int     conv;
{
        float   *fptr = (float *) floats->list;
        int     index = floats->num;

        int     i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; index++, i++) {
                        fptr[index] = raw[i];
                }

                floats->num = index;
        }
        else {
                for (i=0; i<*n; i++) {
                        raw[i] = fptr[i];
                }
        }
}

/*ARGSUSED*/
void    PDFConvIndexes(ddp, raw, indexes, n, conv)
        GKSC_Ptr        ddp;
        int     *raw;
        Indexes *indexes;
        int     *n;
        int     conv;
{
        int     *ind = (int *) indexes->list;
        int     index = indexes->num;

        int     i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; index++, i++) {
                        ind[index] = raw[i];
                }

                indexes->num = index;
        }
        else {
                for (i=0; i<*n; i++) {
                        raw[i] = ind[i];
                }
        }
}

/*ARGSUSED*/
void    PDFConvRGBs(ddp, raw, rgbs, n, conv)
        GKSC_Ptr        ddp;
        float   *raw;
        RGBs    *rgbs;
        int     *n;
        int     conv;
{
        PDFColor        *color = (PDFColor *) rgbs->list;
        int             index = rgbs->num;

        int     i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; i +=3, index++) {
                        color[index].r = raw[  i];
                        color[index].g = raw[i+1];
                        color[index].b = raw[i+2];
                }

                rgbs->num = index;
        }
        else {
                for (index=0, i=0; i<*n; i +=3, index++) {
                        raw[  i] = color[index].r;
                        raw[i+1] = color[index].g;
                        raw[i+2] = color[index].b;
                }
                /*
                 * on exit, *n is the number of elements written into
                 * the raw array. 
                 */
                *n = i;
        }
}
