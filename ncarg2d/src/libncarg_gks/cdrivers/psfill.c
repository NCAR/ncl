/*
 *      $Id: psfill.c,v 1.8 2008-07-23 17:28:01 haley Exp $
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
 *      File:           ps_fill.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Tue Sep 28 10:43:29 MDT 1993
 *
 *      Description:    This file contains routines for doing software
 *                      fill.  The argument "angle" specifies the angle
 *                      of the fill lines; the spacing between the fill
 *                      lines is extracted from the device dependent data.
 *                      This program is a conversion of the algorithm
 *                      implemented in Fortran in the NCAR Softfill package.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "gksc.h"
#include "gks.h"
#include "common.h"
#include "ps.h"
#include "psddi.h"
#include "ps_device.h"

void PSprint_points(PSddp *, PSPoint *, unsigned, terminator_type);
static int icompar(const void *p1, const void *p2);
static void ascsrt(float xa[], int ip[], int n);

static float *sort_array;

void ps_SoftFill (GKSC *gksc, float angle, float spl)

{
  float   xco, yco, spi, rangle, tmp, smalld=.000001, *rst;
  int     *ind, nnd, nra;
  int     jnd, knd, ipt, ipe, indx1, indx2, previous_point, following_point;
  int     i, isp, ipx, lnd, ip1, ip2, in1, in2, jn1, jn2, jnt;
  int     ocounter, counter_inc=3;
  PSPoint *points, opoint;
  PSddp   *psa;

  psa = (PSddp *) gksc->ddp;
  points = (PSPoint *) (gksc->p).list;
  nra = (gksc->p).num;

  rangle = 0.017453292519943 * angle;   /* converts angle to radians */

  /*
   *  Allocate memory.  
   *
   *    rst --  The first nra elements of the rst array are used to 
   *            store the directed distances of the points in the given
   *            polygon from the base line "xco*x+yco*y=0" (see code below 
   *            for the computation of xco and yco).  The second nra 
   *            elements (starting with rst[nra]) of rst are used to 
   *            store the points of intersection of the current fill line 
   *            with the line segments of the polygon (only one number is 
   *            required since we know the distance of the current fill 
   *            line from the base line).
   *
   *    ind --  The first nra elements of ind are used to store a permutation
   *            vector that orders the directed distances in rst[0] through
   *            rst[nra-1].  The second nra elements of rst, starting with
   *            rst[nra], is a permutation vector ordering the points of
   *            intersection stored in rst[nra], rst[nra+1], ... .  The
   *            third nra elements of ind are pointers to the points of
   *            intersection of the current line with the points of the
   *            input polygon.  These pointers are stored backwards in
   *            ind beginning with ind[3*nra].  Point "n" in this list
   *            refers to the line segment in the original polygon that
   *            begins at the previous point and terminates at point "n".
   */

  rst = (float *) malloc (2 * nra * sizeof(float));
  ind = (int   *) malloc (3 * nra * sizeof(int  ));
  nnd = 3*nra;

  /* 
   *  Compute the constants "xco" and "yco" such that any line having an
   *  equation of the form "xco*x+yco*y=c" will have the desired angle.
   */
  xco = (float) (-sin ((double) rangle));
  yco = (float) ( cos ((double) rangle));

  /* 
   *  Compute the directed distances of the given points from the line
   *  "xco*x+yco*y=0".
   */
  for (i = 0; i < nra; ++i)
    rst[i] = xco * points[i].x + yco * points[i].y;

  /* 
   *  Generate a list of indices of the distances, sorted by increasing
   *  distance.  rst[ind[1]], rst[ind[2]], ... rst[ind[nra]] 
   *  is a list of the directed distances of the given points, in increasing 
   *  numerical order.
   */
  ascsrt (rst, ind, nra);

  /* 
   *  Draw lines at distances "spi" from the line "xco*x+yco*y=0" which are
   *  multiples of "spl" between the smallest and largest point distances.
   *  jnd points to the index of that point having the greatest distance
   *  less than the distance of the last line drawn (initially 0) and knd
   *  points to the end of the list of line segments which the last line
   *  drawn intersected - it is stored backwards at the end of ind - the
   *  initial value specifies that this list is empty.
   */
  jnd = -1;
  knd = nnd;

  /* 
   *  ipt is the index of the next point past the last line drawn and ipe
   *  is the index of the last point.
   */
  ipt = ind[0];
  ipe = ind[nra - 1];
  indx1 = (int) (rst[ipt] / spl) - 1;
  indx2 = (int) (rst[ipe] / spl) + 1;
  ocounter = 0;
  for (isp = indx1; isp <= indx2; isp++)
  {
    spi = (float) isp * spl;

    /* 
     *  Advance jnd to reflect the number of points passed over by the
     *  algorithm and update the list of pointers to intersecting lines.
     */
    while ((jnd < nra - 1) && (spi > rst[ipt]))
    {
      previous_point = (ipt + nra - 1) % nra;
      following_point = (ipt + 1) % nra;
      if (rst[previous_point] < rst[ipt])
      {
        ipx = previous_point;

        /* 
         *  Remove intersecting line
         */
        if (knd < nnd)
        {
          for (i = knd; i < nnd; ++i)
          {
            if (ind[i] == ipx)
            {
              ind[i] = ind[knd];
              ++knd;
              break;
            }
          }
        }
      }
      else if (rst[previous_point] > rst[ipt])
      {

        /* 
         *  Add an intersecting line.
         */
        ipx = previous_point;
        --knd;
        ind[knd] = ipx;
      }
      if (rst[ipt] > rst[following_point])
      {
        ipx = ipt;

        /* 
         *  Remove intersecting line
         */
        if (knd < nnd)
        {
          for (i = knd; i < nnd; ++i)
          {
            if (ind[i] == ipx)
            {
              ind[i] = ind[knd];
              ++knd;
              break;
            }
          }
        }
      }
      else if (rst[ipt] < rst[following_point])
      {

        /* 
         *  Add an intersecting line.
         */
        ipx = ipt;
        --knd;
        ind[knd] = ipx;
      }
      ++jnd;
      ipt = ind[jnd + 1];
    }
    /* 
     *  Compute a set of values representing the intersection points of the
     *  current line with the line segments of the polygon.
     */
    if (knd < nnd)
    {
      lnd = nra - 1;
      if (fabs (xco) > fabs (yco))
      {
        for (i = knd; i < nnd; ++i)
        {
          ip1 = ind[i];
          ip2 = (ind[i] + 1) % nra;
          ++lnd;
          tmp = xco * (points[ip2].x - points[ip1].x) 
                + yco * (points[ip2].y - points[ip1].y);
          if (fabs(tmp) > smalld)
          {
            rst[lnd] = (spi * (points[ip2].y - points[ip1].y) - xco *
              (points[ip1].x*points[ip2].y - points[ip2].x*points[ip1].y))/tmp;
          }
          else
          {
            rst[lnd] = .5*(points[ip1].y + points[ip2].y);
          }
        }
      }
      else
      {
        for (i = knd; i < nnd; ++i)
        {
          ip1 = ind[i];
          ip2 = (ind[i] + 1) % nra;
          ++lnd;
          tmp = xco * (points[ip2].x - points[ip1].x) 
                + yco * (points[ip2].y - points[ip1].y); 
          if (fabs(tmp) > smalld)
          {
            rst[lnd] = (spi * (points[ip2].x - points[ip1].x) + yco *
             (points[ip1].x*points[ip2].y - points[ip2].x*points[ip1].y))/tmp;
          }
          else
          {
            rst[lnd] = .5*(points[ip1].x+points[ip2].x);
          } 
        }
      }

      /* 
       *  Put these values in ascending order.  Actually, once again, 
       *  we set up an index array specifying the order.
       */
      ascsrt (rst + nra, ind + nra, lnd - nra + 1);

      /* 
       *  Draw the line segments specified by the list.
       */
      in1 = nra;
      if (fabs (xco) > fabs (yco))
      {
        while (in1 < lnd)
        {
          jn1 = nra + ind[in1];
          in2 = in1 + 1;
          for(;;)
          {
            jn2 = nra + ind[in2];
            if (in2 >= lnd)
              break;
            jnt = nra + ind[in2 + 1];
            if ((rst[jnt] - rst[jn2]) > smalld)
              break;
            in2 += 2;
          }
          if (rst[jn2] - rst[jn1] > smalld)
          {
            opoint.x = (spi - yco * rst[jn1]) / xco;
            opoint.y = rst[jn1];
            PSprint_points(psa, &opoint, 1, MOVETO);
            opoint.x = (spi - yco * rst[jn2]) / xco;
            opoint.y = rst[jn2];
            PSprint_points(psa, &opoint, 1, LINETO);
            ocounter += counter_inc;
            if (((ocounter-1) % POINTS_PER_LINE == 0) || 
                        (ocounter % POINTS_PER_LINE == 0))
                                fprintf(psa->file_pointer, "\n");
            if (ocounter >= MAX_PATH ) {
                fprintf(psa->file_pointer, " K\n");
                ocounter = 0;
            }           
          }
          in1 = in2 + 1;
        }
      }
      else
      {
        while (in1 < lnd)
        {
          jn1 = nra + ind[in1];
          in2 = in1 + 1;
          for(;;)
          {
            jn2 = nra + ind[in2];
            if (in2 >= lnd)
              break;
            jnt = nra + ind[in2 + 1];
            if (rst[jnt] - rst[jn2] > smalld)
              break;
            in2 += 2;
          }
          if (rst[jn2] - rst[jn1] > smalld)
          {
            opoint.x = rst[jn1];
            opoint.y = (spi - xco * rst[jn1]) / yco;
            PSprint_points(psa, &opoint, 1, MOVETO);
            opoint.x = rst[jn2];
            opoint.y = (spi - xco * rst[jn2]) / yco;
            PSprint_points(psa, &opoint, 1, LINETO);
            ocounter += counter_inc;
            if (((ocounter-1) % POINTS_PER_LINE == 0) ||
                        (ocounter % POINTS_PER_LINE == 0))
                                fprintf(psa->file_pointer, "\n");
            if (ocounter >= MAX_PATH ) {
                fprintf(psa->file_pointer, " K\n");
                ocounter = 0;
            }           
          }
          in1 = in2 + 1;
        }
      }
    }
  }
  free (rst);
  free (ind);
}


/*
 *  Given an array of  n  floating values in  xa, 
 *  ascsrt returns a permutation vector ip such that
 * 
 *   xa[ip[i]] <= xa[ip[j]]
 *         for all i,j such that  1 <= i <= j <= n .
 *
 *  This function uses the C library function qsort.
 */

static void ascsrt(float xa[], int ip[], int n)

{
        int i;

        if (n <= 0) {
                return;
        }
        else if (n == 1) {
                ip[0] = 0;
                return;
        }

        sort_array = xa;

        for (i = 0; i < n; i++) {
                ip[i] = i;
        }

        qsort ( (void *) ip, n, sizeof(ip[0]), icompar);
        return;
}

static int icompar(const void *p1, const void *p2)
{
        float difference;
        int *i1,*i2;
        i1 = (int *) p1;
        i2 = (int *) p2;
        difference = sort_array[*i1] - sort_array[*i2];
        if (difference < 0.0) return (-1);
        if (difference > 0.0) return ( 1);
        return (0);
}
