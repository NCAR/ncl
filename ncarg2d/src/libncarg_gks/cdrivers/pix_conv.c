/*
 *      $Id: pix_conv.c,v 1.2 2008-07-23 17:28:00 haley Exp $
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
 *      File:           pixconv.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed May  1 17:49:30 MDT 1991
 *
 *      Description:    These are the PIX conversion routines which convert
 *                      raw data to its device dependent format.
 *                      These routines assume sufficient space exists in the
 *                      destination list. 
 */

#include <X11/Xlib.h>
#include "gksc.h"
#include "pixddi.h"
#include "pix_device.h"


/*ARGSUSED*/
void    PIX_ConvPoints(ddp, rawx, rawy, points, n, conv)
        GKSC_Ptr        ddp;
        float   *rawx, *rawy;
        Points  *points;
        int     *n;
        int     conv;
{
        PIXddp    *xi = (PIXddp *) ddp;
        XPoint  *xpoint_ptr = (XPoint *) points->list; 

        register int    index = points->num;
        
        register int    i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; index++, i++) {
                        /*SUPPRESS55*/
                        xpoint_ptr[index].x = (xi->transform.x_scale * rawx[i])
                                + (int) xi->transform.x_trans;
                        /*SUPPRESS55*/
                        xpoint_ptr[index].y = (xi->transform.y_scale * rawy[i])
                                + (int) xi->transform.y_trans;
                }
                points->num = index;
        }
        else {
                for (i=0; i<*n; i++) {
                        rawx[i] = (xpoint_ptr[i].x - 
                                xi->transform.x_trans) / xi->transform.x_scale;

                        rawy[i] = (xpoint_ptr[i].y - 
                                xi->transform.y_trans) / xi->transform.y_scale;
                }
        }
}

/*ARGSUSED*/
void    PIX_ConvString(ddp, raw, string, n, conv)
        GKSC_Ptr        ddp;
        int     *raw;
        String  *string;
        int     *n;
        int     conv;
{
        char    *s = (char *) string->list;

        register int    index = string->num;

        register int    i;

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
void    PIX_ConvInts(ddp, raw, ints, n, conv)
        GKSC_Ptr        ddp;
        int     *raw;
        Ints    *ints;
        int     *n;
        int     conv;
{
        int     *iptr = (int *) ints->list;
        register int    index = ints->num;

        register int    i;

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
void    PIX_ConvFloats(ddp, raw, floats, n, conv)
        GKSC_Ptr        ddp;
        float   *raw;
        Floats  *floats;
        int     *n;
        int     conv;
{
        float   *fptr = (float *) floats->list;
        register int    index = floats->num;

        register int    i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; i++, index++) {
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
void    PIX_ConvIndexes(ddp, raw, indexes, n, conv)
        GKSC_Ptr        ddp;
        int     *raw;
        Indexes *indexes;
        int     *n;
        int     conv;
{
        int     *ind = (int *) indexes->list;
        register int    index = indexes->num;

        register int    i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; i++, index++) {
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
void    PIX_ConvRGBs(ddp, raw, rgbs, n, conv)
        GKSC_Ptr        ddp;
        float   *raw;
        RGBs    *rgbs;
        int     *n;
        int     conv;
{
        XColor  *xcolor = (XColor *) rgbs->list;
        register int    index = rgbs->num;

        register int    i;

        if (conv == RAW_TO_COOKED) {
                for (i=0; i<*n; i +=3, index++) {
                        /*
                         * 100 is here to insure we are only using 2 points
                         * of precision. (This helps dithering algorithm)
                         */
                        xcolor[index].red = (unsigned short)
                                ((int)(raw[i] * 100) * (MAX_INTENSITY/100));
                        xcolor[index].green =(unsigned short)
                                ((int)(raw[i+1] * 100) * (MAX_INTENSITY/100));
                        xcolor[index].blue = (unsigned short)
                                ((int)(raw[i+2] * 100) * (MAX_INTENSITY/100));
                        xcolor[index].flags = (DoRed | DoGreen | DoBlue);
                        xcolor[index].pad = '\0';
                }

                rgbs->num = index;
        }
        else {
                for (index=0, i=0; i<*n; i +=3, index++) {
                        raw[i] = (float) xcolor[index].red / MAX_INTENSITY;
                        raw[i+1] = (float) xcolor[index].green / MAX_INTENSITY;
                        raw[i+2] = (float) xcolor[index].blue / MAX_INTENSITY;
                }
                /*
                 * on exit, *n is the number of elements written into
                 * the raw array. 
                 */
                *n = i;
        }
}
