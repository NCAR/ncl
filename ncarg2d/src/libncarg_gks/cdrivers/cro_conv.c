/*
 *      $Id: cro_conv.c,v 1.1 2009-04-08 23:25:41 fred Exp $
 */

/*
 *      File:           cro_conv.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Mar  7 11:55:16 MST 2008
 *
 *      Description:    These are the conversion routines that convert 
 *                      raw input data coming in from GKS to their 
 *                      device dependent formats. These routines assume 
 *                      sufficient space exists in the destination list. 
 */

#include <stdio.h>
#include "gksc.h"
#include "cro.h"
#include "croddi.h"
#include "cro_device.h"


/*ARGSUSED*/
void    cro_ConvPoints(GKSC_Ptr ddp, float *rawx, float *rawy, 
                       Points *points, int *n, int conv)
{
  CROddp   *psa = (CROddp *) ddp;
  CROPoint *cro_point_ptr = (CROPoint *) points->list; 
  int      index = points->num;
  int     i;

  if (conv == RAW_TO_COOKED) {
    for (i=0; i<*n; index++, i++) {
      cro_point_ptr[index].x = 
        ( ( (psa->transform).x_scale * rawx[i]) + (psa->transform).x_trans);
      cro_point_ptr[index].y = 
        ( ( (psa->transform).y_scale * rawy[i]) + (psa->transform).y_trans);
    }
    points->num = index;
  }
  else {
    for (i=0; i<*n; i++) {
      rawx[i] = (cro_point_ptr[i].x-(psa->transform).x_trans) /
                (psa->transform).x_scale;
      rawy[i] = (cro_point_ptr[i].y-(psa->transform).y_trans) /
                (psa->transform).y_scale;
      }
   }
}

/*ARGSUSED*/
void cro_ConvString(GKSC_Ptr ddp, int *raw, String *string, int *n, int conv) {

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
void    cro_ConvInts(ddp, raw, ints, n, conv)
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
void    cro_ConvFloats(ddp, raw, floats, n, conv)
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
void    cro_ConvIndexes(ddp, raw, indexes, n, conv)
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
void    cro_ConvRGBs(ddp, raw, rgbs, n, conv)
        GKSC_Ptr        ddp;
        float   *raw;
        RGBs    *rgbs;
        int     *n;
        int     conv;
{
        CROColor *color = (CROColor *) rgbs->list;
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
