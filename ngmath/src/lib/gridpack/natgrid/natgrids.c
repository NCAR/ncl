/*
 * $Id: natgrids.c,v 1.10 2000-08-25 23:29:43 fred Exp $
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

#include "nnghead.h"
#include "nngheads.h"
#include "nnexver.h"

float *c_natgrids(int n, float x[], float y[], float z[],
                  int nxi, int nyi, float xi[], float yi[], int *ier)
{  
   float **data_out=NULL, *rtrn_val=NULL;

   *ier = 0;

   if (single_point == 0)
   {
      asflag = 1;
      Initialize(n, x, y, nxi, nyi, xi, yi);

      if (ReadData(n,x,y,z) != 0)
      {
         *ier = error_status;
         return ( (float *) NULL);
      }
   }

   if (adf)
   {
      CircOut();
      if (error_status)
      {
         *ier = error_status;
         return ( (float *) NULL);
      }
   }
   if (igrad)
   {
      Gradient();
      if (error_status)
      {
         *ier = error_status;
         return ( (float *) NULL);
      }
   }

   data_out = MakeGrid(nxi, nyi, xi, yi);
   if (error_status)
   {
      if((data_out != NULL)&&(data_out[0] !=NULL)) {
	free(data_out[0]);
	free(data_out);
      }
      *ier = error_status;
      return ( (float *) NULL);
   }

   if (single_point == 0)
   {
      Terminate();
   }

   horilap = -1.;
   vertlap = -1.;

   rtrn_val = data_out[0];
   free(data_out);
   return (rtrn_val);
}
void Initialize(int n, float x[], float y[], int nxi, int nyi, 
                float xi[], float yi[])
{

   float xil, xir, yib, yit;

/*
 *  Reserve memory for returning natural neighbor indices 
 *  and associated weights when requested in single point
 *  mode for linear interpolation.
 */
   nbrs = (int *) calloc(n,sizeof(int));
   wts  = (double *) calloc(n,sizeof(double));

   error_status = 0;
   datcnt       = 0;
   magx_orig    = magx;
   magy_orig    = magy;
   magz_orig    = magz;
   iscale       = 0;
   magx_auto    = 1.;
   magy_auto    = 1.;
   magz_auto    = 1.;

/*
 *  Find the limits of the output array.
 */
   xstart       = armin(nxi, xi);
   xend         = armax(nxi, xi);
   ystart       = armin(nyi, yi);
   yend         = armax(nyi, yi);

/*
 *  Find the limits of the input array.
 */
   xil          = armin(n, x);
   xir          = armax(n, x);
   yib          = armin(n, y);
   yit          = armax(n, y);

/*
 *  As the default (that is, unless horizontal and vertical overlaps
 *  have been specifically set by the user) choose the overlap values
 *  as the smallest values that will make all input data points included
 *  in the overlap region.
 */
   if (horilap EQ -1.) {
     if ( (xstart >= xil) && (xend <= xir) ) {
       horilap = 1.01 * (((xstart-xil) < (xir-xend)) ?
                          (xir-xend) : (xstart-xil));
     }
     else if ( (xstart >= xil) && (xend >= xir) ) {
       horilap = 1.01 * (xstart-xil);
     }
     else if ( (xstart <= xil) && (xend <= xir) ) {
       horilap = 1.01 * (xir-xend);
     }
     else if ( (xstart <= xil) && (xir <= xend) ) {
       horilap = 0.;
     }
   }
   if (horilap <= EPSILON) {
     horilap = 0.01 * (xend - xstart);
   }
   if (vertlap EQ -1.) {
     if ( (yib <= ystart) && (yend <= yit) ) {
       vertlap = 1.01 * (((ystart-yib) < (yit-yend)) ?
                          (yit-yend) : (ystart-yib));
     }
     else if ( (ystart <= yib) && (yend <= yit) ) {
       vertlap = 1.01 * (yit-yend);
     }
     else if ( (yib <= ystart) && (yit <= yend) ) {
       vertlap = 1.01 * (ystart-yib);
     }
     else if ( (ystart <= yib) && (yit <= yend) ) {
       vertlap = 0.;
     }
   }
   if (vertlap <= EPSILON) {
     vertlap = 0.01 * (yend - ystart);
   }
}

double armin(int num, float *x)
{
   int i;
   float amin;
   amin = x[0];
   for (i = 1 ; i < num ; i++)
      if (x[i] < amin) amin = x[i];
   return(amin);
}
double armax(int num, float *x)
{
   int i;
   float amax;
   amax = x[0];
   for (i = 1 ; i < num ; i++)
      if (x[i] > amax) amax = x[i];
   return(amax);
}
