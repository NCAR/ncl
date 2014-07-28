/*
 * $Id: nncrunchd.c,v 1.14 2008/07/27 03:10:12 haley Exp $
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

#include "nncheadd.h"
#include "nnchead.h"
#include "nntypes.h"
#include "nnexver.h"
#include "nnuheadd.h"
#include "nnuhead.h"

int ReadDatad(int numdat, double *xin, double *yin, double *zin)
{  
   double temp[3], minx, maxx, miny, maxy, xtmp, ytmp, ztmp;
   double qtxy, qtyx, qtzx, qtzy;
   int i0, i1, n0;

   bigtri[0][0] = bigtri[0][1] = bigtri[1][1] = bigtri[2][0] = -1;
   bigtri[1][0] = bigtri[2][1] = 5;

   if (rootdat EQ NULL) 
   {  
      rootdat  = IMakeDatum();
      if (error_status) return (error_status);

      rootsimp = IMakeSimp();
      if (error_status) return (error_status);

      roottemp = IMakeTemp();
      if (error_status) return (error_status);

      rootneig = IMakeNeig();
      if (error_status) return (error_status);

      rootdat->values[0] = rootdat->values[1]
                         = rootdat->values[2]
                         = 0;
   }
   else 
   {  
      FreeVecti(jndx);
      FreeMatrixd(points);
      FreeMatrixd(joints);
   }
   curdat = rootdat;
   datcnt = 0;
   minx = xstart - horilap;   maxx = xend + horilap;
   miny = ystart - vertlap;   maxy = yend + vertlap;

   for (n0 = 0 ; n0 < numdat ; n0++) {
      temp[0] = xin[n0];
      temp[1] = yin[n0];
      temp[2] = zin[n0];
      if (temp[0] > minx AND temp[0] < maxx AND 
          temp[1] > miny AND temp[1] < maxy) {
          if (curdat->nextdat EQ NULL) 
          {
             curdat->nextdat = IMakeDatum();
             if (error_status) return (error_status);
          }
          curdat = curdat->nextdat;
          datcnt++;
          for (i1 = 0; i1 < 3; i1++) 
             curdat->values[i1] = temp[i1];
      }
   }

   if (datcnt > 3)
   {   
      datcnt3 = datcnt + 3;
      jndx = IntVect(datcnt3);
      if (error_status) return (error_status);
      sumx = sumy = sumz = sumx2 = sumy2 = sumxy = sumxz = sumyz = 0;
      iscale = 0;
/*
 *  Calculate minimums and maximums of the input data accounting for
 *  the scale factors.
 *
 *  For the initial calculations, we have:
 *
 *      maxxy[0][0] = maximum x input data value
 *      maxxy[1][0] = minimum x input data value
 *      maxxy[0][1] = maximum y input data value
 *      maxxy[1][1] = minimum y input data value
 *      maxxy[0][2] = maximum z input data value
 *      maxxy[1][2] = minimum z input data value
 *
 */

data_limits:

      maxxy[0][0] =  maxxy[0][1] = maxxy[0][2] = 
                   -(maxxy[1][0] = maxxy[1][1] = maxxy[1][2] = BIGNUM);
      curdat = rootdat->nextdat;
      for (i0 = 0; i0 < datcnt; i0++)
      {
         xtmp = curdat->values[0] * magx;
         if (maxxy[0][0] < xtmp) 
             maxxy[0][0] = xtmp;  
         if (maxxy[1][0] > xtmp) 
             maxxy[1][0] = xtmp;  
         ytmp = curdat->values[1] * magy;
         if (maxxy[0][1] < ytmp) 
             maxxy[0][1] = ytmp;  
         if (maxxy[1][1] > ytmp) 
             maxxy[1][1] = ytmp;  
         ztmp = curdat->values[2] * magz;
         if (maxxy[0][2] < ztmp) 
             maxxy[0][2] = ztmp; 
         if (maxxy[1][2] > ztmp) 
             maxxy[1][2] = ztmp; 
         curdat  = curdat->nextdat;
      }
/*
 *  Modify the mins and maxs based on the scale factors and overlap regions.
 *  to get the actual minimums and maximums of the data under consideration.
 */
      if (maxxy[0][0] < maxx * magx) 
          maxxy[0][0] = maxx * magx; 
      if (maxxy[1][0] > minx * magx) 
          maxxy[1][0] = minx * magx; 
      if (maxxy[0][1] < maxy * magy) 
          maxxy[0][1] = maxy * magy; 
      if (maxxy[1][1] > miny * magy) 
          maxxy[1][1] = miny * magy; 
/*
 *  Calculate the extents in x, y, and z.
 *
 *      maxxy[0][0] = maximum x extent, including overlap regions.
 *      maxxy[0][1] = maximum y extent, including overlap regions.
 *      maxxy[0][2] = maximum z extent.
 */
      for (i0 = 0 ; i0 < 3 ; i0++) 
      {
         maxxy[0][i0] -= maxxy[1][i0];
      }
      maxhoriz = maxxy[0][0]; 
      if (maxhoriz < maxxy[0][1]) 
          maxhoriz = maxxy[0][1];
      wbit   = maxhoriz * EPSILON;
/*
 *  Calculate the ratio of the x extent by the y extent (qtxy) and
 *  the y extent by the x extent (qtyx) .
 */
      qtxy   = maxxy[0][0] / maxxy[0][1];
      qtyx  = 1./qtxy;
      if ( (qtxy > (2.+EPSILON)) OR (qtyx > (2.+EPSILON)) )
      {
         if (auto_scale) 
         {
/*
 *  Readjust the scaling and recompute the data limits.
 */ 
            iscale = 1;
            if (qtxy > (2+EPSILON) )
            {
               magy *= qtxy;
            }
            else
            {
               magx *= qtyx;
            }
            magx_auto = magx;
            magy_auto = magy;
            magz_auto = magz;
            goto data_limits;
         }
         else
         {
/*
 *  Issue a warning and turn off gradient estimation.
 */
            TooNarrow();
         }
      }

      if (igrad)
      {  
         qtzx = maxxy[0][2] / maxxy[0][0];
         qtzy = maxxy[0][2] / maxxy[0][1];
         if ( (qtzx > 60) OR (qtzy > 60) )
         {
            if (auto_scale) 
            {
/*
 *  Readjust the scaling and recompute the data limits.  The X and Y
 *  scales have been appropriately adjusted by the time you get here,
 *  so dividing magz by either qtzx or qtzy will bring it in line.
 */ 
               iscale = 1;
               magz *= 1./qtzx;
               magx_auto = magx;
               magy_auto = magy;
               magz_auto = magz;
               goto data_limits;
            }
            else
            {
/*
 *  Issue a warning and turn off gradient estimation.
 */
               TooSteep();
            }
         }
         if ( (qtzx < .017) OR (qtzy < .017) )
         {
            if (auto_scale) 
            {
/*
 *  Readjust the scaling and recompute the data limits.  The X and Y
 *  scales have been appropriately adjusted by the time you get here,
 *  so dividing magz by either qtzx or qtzy will bring it in line.
 */ 
               iscale = 1;
               magz *= 1./qtzx;
               magx_auto = magx;
               magy_auto = magy;
               magz_auto = magz;
               goto data_limits;
            }
            else
            {
/*
 *  Issue a warning and turn off gradient estimation.
 */
               TooShallow();
            }
         }
      }

      if (igrad) 
      {
         points = DoubleMatrix(datcnt+4, 6);
         if (error_status) return (error_status);
      }
      else
      {
         points = DoubleMatrix(datcnt+4, 3);
         if (error_status) return (error_status);
      }
      joints = DoubleMatrix(datcnt3, 2); 
      if (error_status) return (error_status);
      curdat = rootdat->nextdat;
      rootdat->nextdat = NULL;
      free(rootdat);
      for (i0 = 0; i0 < datcnt; i0++)
      {  sumx += points[i0][0] = 
            curdat->values[0] * magx;
         sumx2 += SQ(points[i0][0]);
         sumy += points[i0][1] = 
            curdat->values[1] * magy;
         sumy2 += SQ(points[i0][1]);
         sumxy += points[i0][0] * points[i0][1];
         if (densi) points[i0][2] = 1;
         else
         {  sumz += points[i0][2] = 
               curdat->values[2] * magz;
            sumxz += points[i0][0] * points[i0][2];
            sumyz += points[i0][1] * points[i0][2];
         }
         holddat = curdat;
         curdat = curdat->nextdat;
         free(holddat);
      }
      det = (datcnt * (sumx2 * sumy2 - sumxy * sumxy))
          - (sumx * (sumx * sumy2 - sumy * sumxy))
          + (sumy * (sumx * sumxy - sumy * sumx2));
      aaa = ((sumz * (sumx2 * sumy2 - sumxy * sumxy))
          - (sumxz * (sumx * sumy2 - sumy * sumxy))
          + (sumyz * (sumx * sumxy - sumy * sumx2))) / 
         det;
      bbb = 
         ((datcnt * (sumxz * sumy2 - sumyz * sumxy))
          - (sumz * (sumx * sumy2 - sumy * sumxy))
          + (sumy * (sumx * sumyz - sumy * sumxz))) / 
         det;
      ccc = 
         ((datcnt * (sumx2 * sumyz - sumxy * sumxz))
          - (sumx * (sumx * sumyz - sumy * sumxz))
          + (sumz * (sumx * sumxy - sumy * sumx2))) / 
         det;


      for (i0 = 0 ; i0 < 3 ; i0++)
      {  points[datcnt+i0][0] = maxxy[1][0] + 
            bigtri[i0][0] * maxxy[0][0] * RANGE;
         points[datcnt+i0][1] = maxxy[1][1] + 
            bigtri[i0][1] * maxxy[0][1] * RANGE;
         if (densi) 
            points[datcnt+i0][2] = 1;
         else 
            points[datcnt+i0][2] =
            aaa + bbb * points[datcnt+i0][0] + 
            ccc * points[datcnt+i0][1];
      }
      rootdat = NULL;
   }
   else
   {  
      ErrorHnd(1, "ReadData", stderr, "\n");
      error_status = 1;
      return (error_status);
   }

/*
 *  Determine if any input data coordinates are duplicated.
 *  [this is now handled at the top level; the code is left
 *  here commented out for historical purposes.]
 * 
 * if (nndup == 1) {
 *    for (i0 = 0 ; i0 < datcnt ; i0++) {
 *       for (i1 = i0+1 ; i1 < datcnt ; i1++) {
 *          if ( (points[i0][0] == points[i1][0]) &&
 *             (points[i0][1] == points[i1][1]) )
 *          {
 *             sprintf(emsg,"\n  Coordinates %d and %d are identical.\n",i0,i1);
 *             ErrorHnd(2, "ReadData", stderr, emsg);
 *             error_status = 2;
 *             return (error_status);
 *          }
 *       }
 *    }
 * }
 */

/*
 *  Introduce a small random perturbation into the coordinate values.
 */
   srand(367);     
   for (i0 = 0 ; i0 < datcnt ; i0++)
   {
      for (i1 = 0 ; i1 < 2 ; i1++)
      {
         points[i0][i1] += wbit * (0.5 - (double)rand() / RAND_MAX);
      }
   }
   if (sdip OR igrad)
   {  
      piby2 = 2 * atan(1.0);
      nn_pi = piby2 * 2;
      piby32 = 3 * piby2;
      rad2deg = 90 / piby2;
   }
   return (0);
}

double **MakeGridd(int nxi, int nyi, double *xi, double *yi)
{  
   double wxd, wyd, wxde, wydn, surf, surfe, surfn, aspect, slope;
   int i0, j7, j8;
   static int first_as = 1;
   static double **data_out;

   if (optim) {
      for (i0 = 0 ; i0 < datcnt ; i0++) jndx[i0] = 1;

      if ( (single_point == 0) || (igrad > 0) ) {
        TriNeigh();
      }
      else {
        if (first_single == 1) {
          TriNeigh();
          first_single = 0;
        }
      }

      if (error_status) return ( (double **) NULL);
   }

   data_out = DoubleMatrix(nxi,nyi);
   if (error_status) return ( (double **) NULL);

   if (sdip) {
      if (first_as) 
         first_as = 0; 
      else {
         FreeMatrixd(curasd.aspect_outd);
         FreeMatrixd(curasd.slope_outd);
      }
      curasd.crows = 0;
      curasd.ccols = 0;
      curasd.aspect_outd = DoubleMatrix(nxi,nyi);
      curasd.slope_outd = DoubleMatrix(nxi,nyi);
   }

/*
 * jwts flags saving the neighbor indices and associated
 * weights when requested in single point mode using linear interpolation.
 */
   jwts = 0;
   for (j8 = 0 ; j8 < nyi ; j8++) {
      if (updir > 0) 
         wyd = yi[j8]*magy;
      else
         wyd = yi[nyi-j8-1]*magy;

      points[datcnt3][1] = wyd;

      for (j7 = 0 ; j7 < nxi ; j7++) {
         wxd = xi[j7]*magx;
         points[datcnt3][0] = wxd;

         if (!optim) {
            FindNeigh(datcnt3);
            if (error_status) return ( (double **) NULL);
            TriNeigh();
            if (error_status) return ( (double **) NULL);
         }
         FindProp(wxd,wyd);
         if (error_status) return ( (double **) NULL);
         if (!extrap AND !goodflag) 
            surf = nuldat;
         else {
           if(single_point==1 && j7==1 && j8==1 && igrad==0) {
              jwts = 1;
            }
            surf = GridSurface();
            jwts = 0;
            if (igrad>0) surf = Meld(surf,wxd,wyd);
            if (non_neg) if (surf < 0) surf = 0;
         }
         if (sdip) {  
            wxde = wxd + wbit;
            FindProp(wxde,wyd);
            if (error_status) return ( (double **) NULL);
            surfe = GridSurface();
            if (igrad > 0) 
               surfe = Meld(surfe,wxde,wyd);
            if (non_neg) if (surfe < 0) surfe = 0;
            wydn = wyd + wbit;
            FindProp(wxd,wydn);
            if (error_status) return ( (double **) NULL);
            surfn = GridSurface();
            if (igrad > 0) 
               surfn = Meld(surfn,wxd,wydn);
            if (non_neg) if (surfn < 0) surfn = 0;
            surfe = (surf - surfe) / wbit;
            surfn = (surf - surfn) / wbit;
            if (surfe > 0) {  
               if (surfn > 0) 
                  aspect = piby2 - atan(surfn / surfe);
               else 
                  aspect = piby2 + atan(surfn / surfe) * -1;
            }
            else {  
               if (surfe < 0) {  
                  if (surfn > 0) 
                     aspect = piby32 + atan(surfn / surfe) * -1;
                  else aspect = 
                     piby32 - atan(surfn / surfe);
               }
               else {  
                  if (surfn > 0) 
                     aspect = 0; 
                  else 
                     aspect = nn_pi;
               }
            }
            slope = atan(sqrt(SQ(surfe) + SQ(surfn)));
            if (!rads) {  
               aspect *= rad2deg;
               slope *= rad2deg;
            }
            (curasd.aspect_outd)[j7][j8] = aspect;
            (curasd.slope_outd)[j7][j8] = slope;
            curasd.crows = nxi;
            curasd.ccols = nyi;
            if (magz EQ 1. OR (!extrap AND !goodflag))
               data_out[j7][j8] = surf;
            else 
               data_out[j7][j8] = surf/magz;
         }
         else {
            if (magz EQ 1. OR (!extrap AND !goodflag))
               data_out[j7][j8] = surf;
            else 
               data_out[j7][j8] = surf/magz;
         }
      }  
   }     
   return (data_out);
}

void c_nngetsloped(int row, int col, double *slope, int *ier)
{
   if (asflag == 0) {
     error_status = 28;
     ErrorHnd(error_status, "c_nngetsloped", stderr, "\n");
     *ier = 28;
     *slope = -999.;
     return;
   }
   if (iscale == 1)
   {
     sprintf(emsg,"\n\n       Current automatically computed scaling "
                  "values:\n"
                  "         magx = %f\n         magy = %f\n"
                  "         magz = %f\n\n",
                  magx_auto, magy_auto, magz_auto);
     ErrorHnd(26, "c_nngetsloped", stderr, emsg);
     *ier = 26;
     *slope = -999.;
     return;
   }
   if (curasd.crows == 0) 
   {
     ErrorHnd(19, "c_nngetsloped", stderr, "\n");
     *ier = 19;
     *slope = -999.;
     return;
   }
   if (row >= curasd.crows || row < 0) 
   {
     sprintf(emsg,"\n  Requested row = %d (indices starting with one)\n",row+1);
     ErrorHnd(20, "c_nngetsloped", stderr, emsg);
     *ier = 20;
     *slope = -999.;
     return;
   }
   if (col >= curasd.ccols || col < 0) 
   {
     sprintf(emsg,"\n  Requested column = %d (indices starting with one)\n",
               col+1);
     ErrorHnd(21, "c_nngetsloped", stderr, emsg);
     *ier = 21;
     *slope = -999.;
     return;
   }
   *ier = 0;
   *slope = (curasd.slope_outd)[row][col];
}
void c_nngetaspectd(int row, int col, double *aspect, int *ier)
{
   if (asflag == 0) {
     error_status = 28;
     ErrorHnd(error_status, "c_nngetaspectd", stderr, "\n");
     *ier = 28;
     *aspect = -999.;
     return;
   }
   if (iscale == 1)
   {
     sprintf(emsg,"\n\n       Current automatically computed scaling "
                  "values:\n"
                  "         magx = %f\n         magy = %f\n"
                  "         magz = %f\n\n",
                  magx_auto, magy_auto, magz_auto);
     ErrorHnd(25, "c_nngetaspectd", stderr, emsg);
     *ier = 25;
     *aspect = -999.;
     return;
   }
   if (curasd.crows == 0)
   {
     ErrorHnd(22, "c_nngetaspectd", stderr, "\n");
     *ier = 22;
     *aspect = -999.;
     return;
   }
   if (row >= curasd.crows || row < 0)
   {
     sprintf(emsg,"\n  Requested row = %d (indices starting with one)\n",row+1);
     ErrorHnd(20, "c_nngetaspectd", stderr, emsg);
     *ier = 20;
     *aspect = -999.;
     return;
   }
   if (col >= curasd.ccols || col < 0)
   {
     sprintf(emsg,"\n  Requested column = %d (indices starting with one)\n",
               col);
     ErrorHnd(21, "c_nngetaspectd", stderr, emsg);
     *ier = 21;
     *aspect = -999.;
     return;
   }
   *ier = 0;
   *aspect = (curasd.aspect_outd)[row][col];
}

/*
 *  Initialize single point interpolation mode.  This just
 *  does the regridding initialization and initial data analysis.
 */
void c_nnpntinitd(int n, double x[], double y[], double z[])
{
#define NXI 2
#define NYI 2

   double xi[NXI], yi[NYI], wtmp;

   single_point = 1; 
   first_single = 1;
   asflag = 0;
   horilap_save = horilap;
   vertlap_save = vertlap;
   horilap = -1.;
   vertlap = -1.;

/*
 *  Establish the gridded region to contain all of the input
 *  data points plus an extra 10% space around the border.
 */
   xi[0] = armind(n, x);
   xi[1] = armaxd(n, x);
   wtmp  = xi[1] - xi[0];
   xi[0] -= 0.1*wtmp;
   xi[1] += 0.1*wtmp;

   yi[0] = armind(n, y);
   yi[1] = armaxd(n, y);
   wtmp  = yi[1] - yi[0];
   yi[0] -= 0.1*wtmp;
   yi[1] += 0.1*wtmp;

   Initialized(n, x, y, NXI, NYI, xi, yi);

   if (ReadDatad(n,x,y,z) != 0) 
   {
      ErrorHnd(error_status, "c_nnpntinitd", stderr, "\n");
   }
}
void c_nnpntd(double x, double y, double *point)
{
   int   idum, nxi=3, nyi=3, ierr;
   double xdum[1], ydum[1], zdum[1], xi[3], yi[3], *out;

/*
 *  Check to see if the input point is within the gridded region
 *  set up in the initialization.
 */
   if ( (x < xstart) || (x > xend) || (y < ystart) || (y > yend) )
   {
      sprintf(emsg,"\n  Coordinate = (%f, %f)\n", x, y);
      ErrorHnd(27, "c_nnpntd", stderr, emsg);
      return;
   } 
 
/*
 *  Set up a 3 x 3 gridded region with the desired coordinate in
 *  the middle.
 */
   xi[0] = x-0.05*(xend-xstart);
   xi[1] = x;
   xi[2] = x+0.05*(xend-xstart);
   yi[0] = y-0.05*(yend-ystart);
   yi[1] = y;
   yi[2] = y+0.05*(yend-ystart);

   out = c_natgridd(idum, xdum, ydum, zdum, nxi, nyi, xi, yi, &ierr);
   if (ierr != 0)
   {
      ErrorHnd(28, "c_nnpntd", stderr, "\n");
      error_status = ierr;
      *point = -999.;
   }
   
   *point = out[3*1 +1];
   free(out);
}
void c_nnpntendd()
{
   if (single_point == 0) {
     ErrorHnd(32, "c_nnpntendd", stderr, "\n");
     error_status = 32;
     return;
   }
   single_point = 0;
   first_single = 0;
   horilap = horilap_save;
   vertlap = vertlap_save;
   Terminate();
}
