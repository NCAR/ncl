/*
 *  Copyright (C) 1991-1996
 *          by
 *    David F. Watson
 */

#include "nncheadd.h"
#include "nnuheadd.h"

int ReadData(int numdat, double *xin, double *yin, double *zin)
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
      ErrorHnd(1, "ReadData", filee, "\n");
      error_status = 1;
      return (error_status);
   }

/*
 *  Determine if any input data coordinates are duplicated.
 */
   if (dup == 1) {
      for (i0 = 0 ; i0 < datcnt ; i0++) {
         for (i1 = i0+1 ; i1 < datcnt ; i1++) {
            if ( (points[i0][0] == points[i1][0]) &&
               (points[i0][1] == points[i1][1]) )
            {
               sprintf(emsg,"\n  Coordinates %d and %d are identical.\n",i0,i1);
               ErrorHnd(2, "ReadData", filee, emsg);
               error_status = 2;
               return (error_status);
            }
         }
      }
   }

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
      pi = piby2 * 2;
      piby32 = 3 * piby2;
      rad2deg = 90 / piby2;
   }
   return (0);
}
void Gradient()
{  int i0, i1, i2, i3;
   double u2, wxd, wyd, wxde, wydn, xc, xe, xn;
   for (i0=0; i0<datcnt; i0++)
   {
      FindNeigh(i0);
      if (error_status) return;
      if (!ext) 
      {  
         TriNeigh();
         if (error_status) return;
         wxd = points[i0][0];
         wyd = points[i0][1];
         FindProp(wxd,wyd);
         if (error_status) return;
         xc = Surface();
         wxde = wxd + wbit;
         FindProp(wxde,wyd);
         if (error_status) return;
         xe = Surface();
         wydn = wyd + wbit;
         FindProp(wxd,wydn);
         if (error_status) return;
         xn = Surface();
         points[i0][3] = (xc - xe) / wbit;
         points[i0][4] = (xc - xn) / wbit;
         asum /= pi; 
         points[i0][5] = 1 - sqrt(asum / 
            (asum + SQ(points[i0][2] - xc)));
      }
      else     
      {
         points[i0][3] = points[i0][4] = points[i0][5] = xx = 0;
         cursimp = rootsimp;
         for (i1 = 0 ; i1 < numtri ; i1++)
         {  cursimp = cursimp->nextsimp;
            for (i2=0; i2<2; i2++) 
               for (i3=0; i3<3; i3++)
                  work3[i2][i3] = 
                     points[cursimp->vert[0]][i3] - 
                     points[cursimp->vert[i2+1]][i3];
            work3[2][0] = work3[0][1] * work3[1][2] - 
               work3[1][1] * work3[0][2];
            work3[2][1] = work3[0][2] * work3[1][0] - 
               work3[1][2] * work3[0][0];
            work3[2][2] = work3[0][0] * work3[1][1] - 
               work3[1][0] * work3[0][1];
            u2 = 1;
            if (work3[2][2]<0) u2 = -1;
            xx += sqrt(SQ(work3[2][0]) + 
               SQ(work3[2][1]) + SQ(work3[2][2]));
            for (i2=0; i2<3; i2++) points[i0][i2+3] += 
               work3[2][i2] * u2;
         }
         xx = 1 - sqrt(SQ(points[i0][3]) + 
            SQ(points[i0][4]) + 
            SQ(points[i0][5])) / xx;
         points[i0][3] /= points[i0][5];
         points[i0][4] /= points[i0][5];
         points[i0][5] = xx; 
      }
   }
   for (i0=0; i0<3; i0++)
   {  points[datcnt+i0][3] = -bbb;
      points[datcnt+i0][4] = -ccc;
      points[datcnt+i0][5] = 1;
   }
}
void FindNeigh(ipt)
int ipt;
{  int i0, i1, i2, i3, j1, j2, j3, j4, j5;
   if (rootsimp->nextsimp EQ NULL) 
   {
      rootsimp->nextsimp = IMakeSimp();
      if (error_status) return;
   }
   cursimp = rootsimp->nextsimp;
   cursimp->vert[0] = datcnt;
   cursimp->vert[1] = datcnt + 1;
   cursimp->vert[2] = datcnt + 2;
   cursimp->cent[0] = cursimp->cent[1] = 0.5;
   cursimp->cent[2] = BIGNUM;
   numtri = 1;
   lasttemp = roottemp;
   for (i2=0; i2<3; i2++)
   {  j1 = 0;
      if (j1 EQ i2) j1++;
      j2 = j1 + 1;
      if (j2 EQ i2) j2++;
      if (lasttemp->nexttemp EQ NULL) 
      {
         lasttemp->nexttemp = IMakeTemp();
         if (error_status) return;
      }
      lasttemp = lasttemp->nexttemp;
      lasttemp->end[0] = cursimp->vert[j1];
      lasttemp->end[1] = cursimp->vert[j2];
   }
   curtemp = roottemp;
   for (i1=0; i1<3; i1++)
   {  curtemp = curtemp->nexttemp;
      for (i2=0; i2<2; i2++)
      {  work3[i2][0] = points[curtemp->end[i2]][0] - 
            points[ipt][0];
         work3[i2][1] = points[curtemp->end[i2]][1] - 
            points[ipt][1];
         work3[i2][2] = work3[i2][0] * 
            (points[curtemp->end[i2]][0] + 
            points[ipt][0]) / 2 + work3[i2][1] * 
            (points[curtemp->end[i2]][1] + 
            points[ipt][1]) / 2;
      }
      xx = work3[0][0] * work3[1][1] - 
         work3[1][0] * work3[0][1];
      cursimp->cent[0] = (work3[0][2] * work3[1][1] - 
         work3[1][2] * work3[0][1]) / xx;
      cursimp->cent[1] = (work3[0][0] * work3[1][2] - 
         work3[1][0] * work3[0][2]) / xx;
      cursimp->cent[2] = SQ(points[ipt][0] - 
         cursimp->cent[0]) + SQ(points[ipt][1] - 
         cursimp->cent[1]);
      cursimp->vert[0] = curtemp->end[0];
      cursimp->vert[1] = curtemp->end[1];
      cursimp->vert[2] = ipt;
      lastsimp = cursimp;
      if (cursimp->nextsimp EQ NULL) 
      {
         cursimp->nextsimp = IMakeSimp();
         if (error_status) return;
      }
      cursimp = cursimp->nextsimp; 
   }
   numtri += 2;
   for (i0=0; i0<datcnt; i0++)
   {  if (i0 NE ipt)
      {  j4 = 0;
         j3 = -1;
         lasttemp = roottemp;
         cursimp = rootsimp;
         for (i1=0; i1<numtri; i1++)
         {  prevsimp = cursimp;
            cursimp = cursimp->nextsimp;
            xx = cursimp->cent[2] - 
               SQ(points[i0][0] - cursimp->cent[0]);
            if (xx > 0)
            {  xx -= SQ(points[i0][1] - 
                  cursimp->cent[1]);
               if (xx > 0)
               { j4--;
                 for (i2=0; i2<3; i2++)
                 { j1 = 0; 
                   if (j1 EQ i2) j1++; 
                   j2 = j1 + 1;    
                   if (j2 EQ i2) j2++;
                   if (j3>1)
                   { j5 = j3;
                     curtemp = roottemp;
                     for (i3=0; i3<=j5; i3++)
                     { prevtemp = curtemp;
                       curtemp =
                          curtemp->nexttemp;
                       if (cursimp->vert[j1] EQ 
                          curtemp->end[0])
                       { if (cursimp->vert[j2] EQ 
                            curtemp->end[1])
                         { if (curtemp EQ lasttemp) 
                              lasttemp = prevtemp;
                            else
                           { prevtemp->nexttemp = 
                                curtemp->nexttemp;
                             curtemp->nexttemp = 
                                lasttemp->nexttemp;
                             lasttemp->nexttemp = 
                                curtemp;
                           }
                           j3--;
                           goto NextOne;
                         }
                       }
                     }
                   }
                   if (lasttemp->nexttemp EQ NULL) 
                   {
                      lasttemp->nexttemp = IMakeTemp();
                      if (error_status) return;
                   }
                   lasttemp = lasttemp->nexttemp;
                   j3++;
                   lasttemp->end[0] = 
                      cursimp->vert[j1];
                   lasttemp->end[1] = 
                      cursimp->vert[j2];
NextOne:; }
                  if (cursimp EQ lastsimp) 
                     lastsimp = prevsimp;
                  else
                  {  prevsimp->nextsimp = 
                        cursimp->nextsimp;
                     cursimp->nextsimp = 
                        lastsimp->nextsimp;
                     lastsimp->nextsimp = cursimp;
                     cursimp = prevsimp;
                  }
               }
            }
         }
         if (j3 > -1)
         {  curtemp = roottemp;
            cursimp = lastsimp->nextsimp;
            for (i1=0; i1<=j3; i1++)
            {  curtemp = curtemp->nexttemp;
               if (curtemp->end[0] EQ ipt OR 
                  curtemp->end[1] EQ ipt)
               {  for (i2=0; i2<2; i2++)
                  {  work3[i2][0] = 
                        points[curtemp->end[i2]][0] - 
                        points[i0][0];
                     work3[i2][1] = 
                        points[curtemp->end[i2]][1] - 
                        points[i0][1];
                     work3[i2][2] = work3[i2][0] * 
                        (points[curtemp->end[i2]][0] + 
                        points[i0][0]) / 2 + 
                        work3[i2][1] *
                        (points[curtemp->end[i2]][1] + 
                        points[i0][1]) / 2;
                  }
                  xx = work3[0][0] * work3[1][1] - 
                     work3[1][0] * work3[0][1];
                  cursimp->cent[0] = (work3[0][2] * 
                     work3[1][1] - work3[1][2] * 
                     work3[0][1]) / xx;
                  cursimp->cent[1] = (work3[0][0] * 
                     work3[1][2] - work3[1][0] * 
                     work3[0][2]) / xx;
                  cursimp->cent[2] = 
                     SQ(points[i0][0] - 
                     cursimp->cent[0]) +
                     SQ(points[i0][1] - 
                     cursimp->cent[1]);
                  cursimp->vert[0] = curtemp->end[0];
                  cursimp->vert[1] = curtemp->end[1];
                  cursimp->vert[2] = i0;
                  lastsimp = cursimp;
                  if (cursimp->nextsimp EQ NULL) 
                  {
                     cursimp->nextsimp = IMakeSimp();
                     if (error_status) return;
                  }
                  cursimp = cursimp->nextsimp; 
                  j4++;
               }
            }
            numtri += j4;
         }
      }
   }
   for (i0=0; i0<datcnt; i0++) jndx[i0] = 0;
   cursimp = rootsimp;
   for (ext=0, i1=0; i1<numtri; i1++)
   {  cursimp = cursimp->nextsimp;
      for (i2=0; i2<3; i2++) 
      {  if (cursimp->vert[i2] < datcnt)
         {  if (cursimp->vert[i2] NE ipt) 
               jndx[cursimp->vert[i2]] = 1;
         }
         else ext = 1; 
      }
   }
}
void TriNeigh()
{  int i0, i1, i2, i3, j1, j2, j3, j4, j5;
   if (rootsimp->nextsimp EQ NULL)
   {
      rootsimp->nextsimp = IMakeSimp();
      if (error_status) return;
   }
   lastsimp = cursimp = rootsimp->nextsimp;
   cursimp->vert[0] = datcnt;
   cursimp->vert[1] = datcnt + 1;
   cursimp->vert[2] = datcnt + 2;
   cursimp->cent[0] = cursimp->cent[1] = 0.5;
   cursimp->cent[2] = BIGNUM;
   numtri = 1;
   for (i0=0; i0<datcnt; i0++)
   {  if (jndx[i0])
      {  j3 = -1;
         lasttemp = roottemp;
         cursimp = rootsimp;
         for (i1=0; i1<numtri; i1++)
         {  prevsimp = cursimp;
            cursimp = cursimp->nextsimp;
            xx = cursimp->cent[2] - 
               SQ(points[i0][0] - cursimp->cent[0]);
            if (xx > 0)
            {  xx -= SQ(points[i0][1] - 
                  cursimp->cent[1]);
               if (xx > 0)
               {  for (i2=0; i2<3; i2++)
                  {  j1 = 0;
                     if (j1 EQ i2) j1++;
                     j2 = j1 + 1;
                     if (j2 EQ i2) j2++;
                     if (j3>1)
                     {  j5 = j3;
                        curtemp = roottemp;
                        for (i3=0; i3<=j5; i3++)
                        { prevtemp = curtemp;
                          curtemp = 
                             curtemp->nexttemp;
                          if (cursimp->vert[j1] EQ 
                             curtemp->end[0])
                          { if (cursimp->vert[j2] EQ 
                               curtemp->end[1])
                            { if (curtemp EQ lasttemp) 
                                 lasttemp = prevtemp;
                              else
                              { prevtemp->nexttemp = 
                                  curtemp->nexttemp;
                                curtemp->nexttemp = 
                                  lasttemp->nexttemp;
                                lasttemp->nexttemp = 
                                  curtemp;
                               }
                               j3--;
                               goto NextOne;
                             }
                           }
                        }
                     }
                     if (lasttemp->nexttemp EQ NULL)
                     {
                        lasttemp->nexttemp = IMakeTemp();
                        if (error_status) return;
                     }
                     lasttemp = lasttemp->nexttemp;
                     j3++;
                     lasttemp->end[0] = 
                        cursimp->vert[j1];
                     lasttemp->end[1] = 
                        cursimp->vert[j2];
NextOne:; }
                  if (cursimp EQ lastsimp) 
                     lastsimp = prevsimp;
                  else
                  {  prevsimp->nextsimp = 
                        cursimp->nextsimp;
                     cursimp->nextsimp = 
                        lastsimp->nextsimp;
                     lastsimp->nextsimp = cursimp;
                     cursimp = prevsimp;
                  }
               }
            }
         }
         curtemp = roottemp;
         cursimp = lastsimp->nextsimp;
         for (i1=0; i1<=j3; i1++)
         {  curtemp = curtemp->nexttemp;
            for (i2=0; i2<2; i2++)
            {  work3[i2][0] = 
                  points[curtemp->end[i2]][0] - 
                  points[i0][0];
               work3[i2][1] = 
                  points[curtemp->end[i2]][1] - 
                  points[i0][1];
               work3[i2][2] = work3[i2][0] * 
                  (points[curtemp->end[i2]][0] + 
                  points[i0][0]) / 2 + work3[i2][1] * 
                  (points[curtemp->end[i2]][1] + 
                  points[i0][1]) / 2;
            }
            xx = work3[0][0] * work3[1][1] - 
               work3[1][0] * work3[0][1];
            cursimp->cent[0] = 
               (work3[0][2] * work3[1][1] - 
               work3[1][2] * work3[0][1]) / xx;
            cursimp->cent[1] = 
               (work3[0][0] * work3[1][2] - 
               work3[1][0] * work3[0][2]) / xx;
            cursimp->cent[2] = SQ(points[i0][0] - 
               cursimp->cent[0]) + SQ(points[i0][1] - 
               cursimp->cent[1]);
            cursimp->vert[0] = curtemp->end[0];
            cursimp->vert[1] = curtemp->end[1];
            cursimp->vert[2] = i0;
            lastsimp = cursimp;
            if (cursimp->nextsimp EQ NULL)
            {
               cursimp->nextsimp = IMakeSimp();
               if (error_status) return;
            }
            cursimp = cursimp->nextsimp;
         }
         numtri += 2;
      }
   }
   cursimp = rootsimp;
   for (asum=0, i0=0; i0<numtri; i0++) 
   {  cursimp = cursimp->nextsimp;
      for (i1=0; i1<2; i1++)
      {  work3[0][i1] = points[cursimp->vert[1]][i1] - 
            points[cursimp->vert[0]][i1];
         work3[1][i1] = points[cursimp->vert[2]][i1] - 
            points[cursimp->vert[0]][i1];
      }
      xx = work3[0][0] * work3[1][1] - 
         work3[0][1] * work3[1][0];
      if (xx < 0)
      {  j4 = cursimp->vert[2];
         cursimp->vert[2] = cursimp->vert[1];
         cursimp->vert[1] = j4;
         if (cursimp->vert[0] < datcnt) 
            asum -= xx / 2;
      }
      else if (cursimp->vert[0] < datcnt) 
         asum += xx / 2;
   }
}
void CircOut()
{
   FILE *filer;
   int ix,i0;
   struct simp *simpaddr;
   if (adf)
   {  
      for (i0 = 0; i0 < datcnt; i0++) jndx[i0] = 1;
      TriNeigh();
      if (error_status) return;

      if ((filer = fopen(tri_file,"w")) EQ (FILE *) NULL) 
      {
         ErrorHnd(3, "CircOut", filee, "\n");
         error_status = 3;
         return;
      }

      /*
       *  Put out defaults for plot control parameters.
       */
      fprintf(filer,"/*\n");
      fprintf(filer,"/* Integer flags (I5 format).\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"    8 - GKS workstation type "
                             "(1=ncgm; 8=X11 window; 20=PostScript).\n");
      fprintf(filer,"    1 - flags whether axes should be drawn.\n");
      fprintf(filer,"    0 - Halfax/Grid flag (0=halfax and 1=grid)\n");
      fprintf(filer,"    1 - Flags whether triangulation should be drawn.\n");
      fprintf(filer,"    0 - Flags whether a blue dot should be drawn "
                             "at (0.,0.) [0=no; 1=yes]\n");
      fprintf(filer,"    0 - Flag to indicate whether the pseudo data "
                             "should be included in the plot.\n");
      fprintf(filer,"    1 - Flag indicating whether the natural "
                             "neighbor circles are drawn.\n");
      fprintf(filer,"    1 - Flags whether the centers of the natural "
                             "neighborhood circles are drawn.\n");
      fprintf(filer,"    1 - Flag indicating if Voronoi polygons should "
                             "be drawn [0=no; 1=yes].\n");
      fprintf(filer,"    1 - Flag indicating if the original points are "
                             "to be marked.\n");

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  Color information (3F7.3 format) as RGB triples\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"  0.000  0.000  0.000 - background color\n");
      fprintf(filer,"  1.000  1.000  1.000 - foreground color "
                                            "(used for axes)\n");
      fprintf(filer,"  1.000  0.000  0.000 - circumcircle color\n");
      fprintf(filer,"  0.000  1.000  0.000 - color of circumcircle "
                                             "centers\n");
      fprintf(filer,"  0.000  1.000  1.000 - color for triangulation\n");
      fprintf(filer,"  1.000  1.000  0.000 - Voronoi polygon color\n");
      fprintf(filer,"  1.000  1.000  0.000 - color of vertex dots\n");
      fprintf(filer,"  0.000  0.000  1.000 - color of reference dot\n");
      fprintf(filer,"  0.000  0.000  1.000 - color for natural neighbor "
                                             "points\n");
      fprintf(filer,"  1.000  1.000  1.000 - color to mark points where "
                                             "natural neighbors are desired\n");

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  Scale factors (F7.3 format)\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"  1.000 - scale factor for dots at vertices\n");
      fprintf(filer,"  1.000 - scale factor for circumcircle centers\n");
      fprintf(filer,"  2.000 - scale factor for circle lines\n");
      fprintf(filer,"  2.000 - scale factor for Voronoi polygon lines\n");
      fprintf(filer,"  2.000 - scale factor for tringulation lines\n");
      fprintf(filer,"  1.000 - scale factor for axes lines\n");
      fprintf(filer,"  1.000 - scale factor for points where natural "
                                            "neighbors are desired\n");
      fprintf(filer,"  1.000 - scale factor for points marking natural "
                                            "neighbors\n");

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  User coordinates for SET call (4E15.3 format), "
                         "defaults if all zeros\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"      0.000E+00      0.000E+00      0.000E+00     "
                    " 0.000E+00\n");

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  Number of user input data. (I5 format)\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"%5d\n",datcnt);

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  User data.  The datum number occurs first "
                         "(in I5 format) followed\n");
      fprintf(filer,"/*  by the x,y,z values (in E15.3 format).\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      for (ix = 0; ix < datcnt; ix++) {
        fprintf(filer,"%5d%15.3E%15.3E%15.3E\n",
                ix+1,points[ix][0],points[ix][1],points[ix][2]);
      }

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  Pseudo data.\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      for (ix = datcnt; ix < datcnt+3; ix++) {
        fprintf(filer,"%5d%15.3E%15.3E%15.3E\n",
                ix+1,points[ix][0],points[ix][1],points[ix][2]);
      }

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  The number of circumcircles (I5 format).\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      simpaddr = rootsimp->nextsimp;
      fprintf(filer,"%5d\n",numtri);

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  Circumcircle data.  The first three numbers are "
                         "the numbers of the\n");
      fprintf(filer,"/*  data (as listed above) lying on the "
                         "circumcircle; the next two\n");
      fprintf(filer,"/*  numbers give the center position of the "
                         "circumcircle; the final\n");
      fprintf(filer,"/*  number is the square of the radius of the "
                         "circumcircle.\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      for (ix = 0; ix < numtri; ix++) {
        fprintf(filer,"%5d%5d%5d%15.3E%15.3E%15.3E\n",
                simpaddr->vert[0]+1,simpaddr->vert[1]+1,simpaddr->vert[2]+1,
                simpaddr->cent[0],simpaddr->cent[1],simpaddr->cent[2]);
        simpaddr = simpaddr->nextsimp;
      }

      fprintf(filer,"/*\n");
      fprintf(filer,"/*  Number of points where natural neighbors are "
                    "to be marked and\n");
      fprintf(filer,"/*  a flag indicating whether just the points where "
                    "first order neighbors\n");
      fprintf(filer,"/*  are desired are marked (-1), whether the first "
                    " order neighbors \n");
      fprintf(filer,"/*  will be marked as well (0), or both first and "
                    "second order neighbors\n");
      fprintf(filer,"/*  are marked (1).  The points will be marked with "
                    "Xs, in the\n");
      fprintf(filer,"/*  color described above. (2I5 format)\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"    0    0\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*  The coordinate list of points whose natural "
                    "neighbors are to\n");
      fprintf(filer,"/*  be displayed (using the color index as described "
                    "above), should\n");
      fprintf(filer,"/*  be listed here in 2E15.3 format.\n");
      fprintf(filer,"/*\n");
      fprintf(filer,"/*..+....1....+....2....+....3....+....4"
                    "....+....5....+....6....+....7....+....8\n");
      fprintf(filer,"/*    0.000E-00      0.000E-00\n");

      fclose(filer);
      return;
   }
}
double **MakeGrid(int nxi, int nyi, double *xi, double *yi)
{  
   double wxd, wyd, wxde, wydn, surf, surfe, surfn, aspect, slope;
   int i0, j7, j8;
   static int first_c = 1, first_as = 1;
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

   if (first_c)
      first_c = 0;
   else
      FreeMatrixd(data_out);

   data_out = DoubleMatrix(nxi,nyi);
   if (error_status) return ( (double **) NULL);

   if (sdip) {
      if (first_as) 
         first_as = 0; 
      else {
         FreeMatrixd(curas.aspect_out);
         FreeMatrixd(curas.slope_out);
      }
      curas.crows = 0;
      curas.ccols = 0;
      curas.aspect_out = DoubleMatrix(nxi,nyi);
      curas.slope_out = DoubleMatrix(nxi,nyi);
   }

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
            surf = Surface();
            if (igrad>0) surf = Meld(surf,wxd,wyd);
            if (non_neg) if (surf < 0) surf = 0;
         }
         if (sdip) {  
            wxde = wxd + wbit;
            FindProp(wxde,wyd);
            if (error_status) return ( (double **) NULL);
            surfe = Surface();
            if (igrad > 0) 
               surfe = Meld(surfe,wxde,wyd);
            if (non_neg) if (surfe < 0) surfe = 0;
            wydn = wyd + wbit;
            FindProp(wxd,wydn);
            if (error_status) return ( (double **) NULL);
            surfn = Surface();
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
                     aspect = pi;
               }
            }
            slope = atan(sqrt(SQ(surfe) + SQ(surfn)));
            if (!rads) {  
               aspect *= rad2deg;
               slope *= rad2deg;
            }
            (curas.aspect_out)[j7][j8] = aspect;
            (curas.slope_out)[j7][j8] = slope;
            curas.crows = nxi;
            curas.ccols = nyi;
            if (magz EQ 1.) 
               data_out[j7][j8] = surf;
            else 
               data_out[j7][j8] = surf/magz;
         }
         else {
            if (magz EQ 1.) 
               data_out[j7][j8] = surf;
            else 
               data_out[j7][j8] = surf/magz;
         }
      }  
   }     
   return (data_out);
}
void FindProp(wxd, wyd)
double wxd, wyd;
{  int i2, i3, i4, pos_count, inside;
   double xx, work3[3][3], work4[3][2];
   lastneig = rootneig;
   goodflag = 0;
   numnei = -1;
   cursimp = rootsimp;
   for (i2=0; i2<numtri; i2++)
   {  cursimp = cursimp->nextsimp;
      xx = cursimp->cent[2] - 
         SQ(wxd - cursimp->cent[0]);
      if (xx > 0)
      {  xx -= SQ(wyd - cursimp->cent[1]);
         if (xx > 0)
         {  inside = 0;
            if (cursimp->vert[0] < datcnt) inside = 1;
            for (i3=0; i3<3; i3++)
            {  for (i4=0; i4<2; i4++)
               {  work3[i4][0] = 
                     points[cursimp->
                     vert[scor[i3][i4]]][0] - wxd;
                  work3[i4][1] = 
                     points[cursimp->
                     vert[scor[i3][i4]]][1] - wyd;
                  work3[i4][2] = work3[i4][0] *
                     (points[cursimp->
                     vert[scor[i3][i4]]][0] + 
                     wxd) / 2 + work3[i4][1] *
                     (points[cursimp->
                     vert[scor[i3][i4]]][1] + 
                     wyd) / 2;
               }
               xx =  work3[0][0] * work3[1][1] - 
                  work3[1][0] * work3[0][1];
               work4[i3][0] = (work3[0][2] * 
                  work3[1][1] - work3[1][2] * 
                  work3[0][1]) / xx;
               work4[i3][1] = (work3[0][0] * 
                  work3[1][2] - work3[1][0] * 
                  work3[0][2]) / xx;
            }
            pos_count = 0;
            for (i3=0; i3<3; i3++)
            {  work3[2][i3] = 
                  ((work4[scor[i3][0]][0] - 
                  cursimp->cent[0]) *
                  (work4[scor[i3][1]][1] - 
                  cursimp->cent[1]) -
                  (work4[scor[i3][1]][0] - 
                  cursimp->cent[0]) *
                  (work4[scor[i3][0]][1] - 
                  cursimp->cent[1])) / 2;
               if (work3[2][i3]>0) pos_count++;
            }
            if (pos_count>2 AND inside) goodflag = 1;
            for (i3=0; i3<3; i3++)
            {  if (numnei>1)
               {  curneig = rootneig;
                  for (i4=0; i4<=numnei; i4++)
                  {  curneig = curneig->nextneig;
                     if (cursimp->vert[i3] EQ 
                        curneig->neinum)
                     {  curneig->narea += 
                           work3[2][i3];
                        goto GOTEM;
                     }
                  }
               }
               if (lastneig->nextneig EQ NULL)
               { 
                  lastneig->nextneig = IMakeNeig();
                  if (error_status) return;
               }
               lastneig = lastneig->nextneig;
               numnei++;
               lastneig->neinum = cursimp->vert[i3];
               lastneig->narea = work3[2][i3];
GOTEM:;       }   
         }
      }
   }
}
double Surface()
{  int i0;
   double xx, asurf;
   curneig = rootneig;
   for (xx=0, i0=0; i0<=numnei; i0++) 
   {  curneig = curneig->nextneig;
      xx += curneig->narea;
   }
   curneig = rootneig;
   for (asurf=0, i0=0; i0<=numnei; i0++)
   {  curneig = curneig->nextneig;
      curneig->narea /= xx;
      asurf += curneig->narea * 
         points[curneig->neinum][2];
   }
   return asurf;
}
double Meld(double asurf, double wxd, double wyd)
{  int i0;
   double rS, rT, rB, bD, bB, hP;
   curneig = rootneig;
   for (i0 = 0 ; i0 <= numnei ; i0++)
   {  
      curneig = curneig->nextneig;
      curneig->coord = 0;
      if (curneig->narea>0.00001 AND curneig->narea < 2)
      {  
         if (fabs(points[curneig->neinum][5]) > 0.00001)
         {  
            rS = fabs(points[curneig->neinum][5]) + bI;
            rT = rS * bJ;
            rB = 1 / rT;
            bD = pow(curneig->narea, rT);
            bB = bD * 2;
            if (bD>0.5) bB = (1 - bD) * 2;
            bB = pow(bB, rS) / 2;
            if (bD>0.5) bB = 1 - bB;
            hP = pow(bB, rB);
            curneig->coord = 
               ((points[curneig->neinum][3] *
               points[curneig->neinum][0] + 
               points[curneig->neinum][4] *
               points[curneig->neinum][1] + 
               points[curneig->neinum][2] -
               points[curneig->neinum][3] * 
               wxd -
               points[curneig->neinum][4] * 
               wyd) - asurf) * hP;
         }
      }
   }
   curneig = rootneig;
   for (i0=0; i0<=numnei; i0++) 
   {  curneig = curneig->nextneig;
      asurf += curneig->coord;
   }
   return asurf; 
}
void TooSteep()
{  
   ErrorHnd(4,"TooSteep", filee, "\n");
   igrad = 0;
}
void TooShallow()
{  
   ErrorHnd(5,"TooShallow", filee, "\n");
   igrad = 0;
}
void TooNarrow()
{  
   ErrorHnd(6, "TooNarrow", filee, "\n");
   igrad = 0;
}
int *IntVect(int ncols)
{  
   int *vectptr;
   if ((vectptr = (int *) malloc(ncols * sizeof(int))) EQ (int *) NULL)
   {  
      error_status = 7;
      ErrorHnd(error_status, "IntVect", filee, "\n");
      vectptr = (int *) NULL;
   }
   return vectptr;
}
void FreeVecti(int *vectptr)
{  
   free(vectptr);
}
double *DoubleVect(int ncols)
{  
   double *vectptr;
   if ((vectptr = (double *) 
      malloc(ncols * sizeof(double))) EQ (double *) NULL)
   {
      error_status = 8;
      ErrorHnd(error_status, "DoubleVect", filee, "\n");
      return ( (double *) NULL);
   }
   return vectptr;
}
void FreeVectd(double *vectptr)
{  
   free(vectptr);
}
int **IntMatrix(int nrows, int ncols)
{  int i0;
   int **matptr;
   if (nrows < 2) nrows = 2;
   if (ncols < 2) ncols = 2;
   if ((matptr = (int **) 
      malloc(nrows * sizeof(int *))) EQ (int **) NULL)
   {  
      error_status = 9;
      ErrorHnd(error_status, "IntMatrix", filee, "\n");
      return ( (int **) NULL);
   }
   if ((matptr[0] = (int *) 
      malloc(nrows * ncols * sizeof(int))) EQ (int *) NULL)
   {
      error_status = 10;
      ErrorHnd(error_status, "IntMatrix", filee, "\n");
      return ( (int **) NULL);
   }
   for (i0=1; i0<nrows; i0++) 
      matptr[i0] = matptr[0] + i0 * ncols;
   return matptr;
}
void FreeMatrixi(int **matptr)
{  
   free(matptr[0]);        /* added 1/1/95 */
   free(matptr);
}
float **FloatMatrix(int nrows, int ncols)
{  int i0;
   float **matptr;
   if (nrows < 2) nrows = 2;
   if (ncols < 2) ncols = 2;
   if ((matptr = (float **) 
      malloc(nrows * sizeof(float *))) EQ (float **) NULL)
   {
      error_status = 11;
      ErrorHnd(error_status, "FloatMatrix", filee, "\n");
      return ( (float **) NULL);
   }
   if ((matptr[0] = (float *) 
      malloc(nrows * ncols * sizeof(float))) EQ (float *) NULL)
   {
      error_status = 12;
      ErrorHnd(error_status, "FloatMatrix", filee, "\n");
      return ( (float **) NULL);
   }
   for (i0=1; i0<nrows; i0++) 
      matptr[i0] = matptr[0] + i0 * ncols;
   return matptr;
}
void FreeMatrixf(float **matptr)
{  
   free(matptr[0]);        /* added 1/1/95 */
   free(matptr);
}
double **DoubleMatrix(int nrows, int ncols)
{  
   int i0;
   double **matptr;
   if (nrows < 2) nrows = 2;
   if (ncols < 2) ncols = 2;
   if ((matptr = (double **) 
      malloc(nrows * sizeof(double *))) EQ (double **) NULL)
   {  
      error_status = 13;
      ErrorHnd(error_status, "DoubleMatrix", filee, "\n");
      return ( (double **) NULL);
   }
   if ((matptr[0] = (double *) 
      malloc(nrows * ncols * sizeof(double))) EQ (double *) NULL)
   {
      error_status = 14;
      ErrorHnd(error_status, "DoubleMatrix", filee, "\n");
      return ( (double **) NULL);
   }
   for (i0 = 1; i0 < nrows; i0++) 
      matptr[i0] = matptr[0] + i0 * ncols;
   return matptr;
}
void FreeMatrixd(double **matptr)
{  
   free(matptr[0]);        /* added 1/1/95 */
   free(matptr);
}
struct datum *IMakeDatum()
{  
   struct datum *datptr;
   if ((datptr = (struct datum *) 
      malloc(sizeof(struct datum))) EQ (struct datum *) NULL)
   {  
      error_status = 15;
      ErrorHnd(error_status, "IMakeDatum", filee, "\n");
      return ((struct datum *) NULL);
   }
   datptr->nextdat = NULL;
   return datptr;
}
struct simp *IMakeSimp()
{  
   struct simp *simpptr;
   if ((simpptr = (struct simp *) 
      malloc(sizeof(struct simp))) EQ (struct simp *) NULL)
   {
      error_status = 16;
      ErrorHnd(error_status, "IMakeSimp", filee, "\n");
      return ((struct simp *) NULL);
   }
   simpptr->nextsimp = NULL;
   return (simpptr);
}
struct temp *IMakeTemp()
{  
   struct temp *tempptr;
   if ((tempptr = (struct temp *) 
      malloc(sizeof(struct temp))) EQ (struct temp *) NULL)
   {
      error_status = 17;
      ErrorHnd(error_status, "IMakeTemp", filee, "\n");
      return ((struct temp *) NULL);
   }
   tempptr->nexttemp = NULL;
   return tempptr;
}
struct neig *IMakeNeig()
{
   struct neig *neigptr;
   if ((neigptr = (struct neig *) 
      malloc(sizeof(struct neig))) EQ (struct neig *) NULL)
   {
      error_status = 18;
      ErrorHnd(error_status, "IMakeNeig", filee, "\n");
      return ((struct neig *) NULL);
   }
   neigptr->nextneig = NULL;
   return neigptr;
}
void c_nngetsloped(int row, int col, double *slope, int *ier)
{
   if (asflag == 0) {
     error_status = 28;
     ErrorHnd(error_status, "c_nngetsloped", filee, "\n");
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
     ErrorHnd(26, "c_nngetsloped", filee, emsg);
     *ier = 26;
     *slope = -999.;
     return;
   }
   if (curas.crows == 0) 
   {
     ErrorHnd(19, "c_nngetsloped", filee, "\n");
     *ier = 19;
     *slope = -999.;
     return;
   }
   if (row >= curas.crows || row < 0) 
   {
     sprintf(emsg,"\n  Requested row = %d (indices starting with one)\n",row+1);
     ErrorHnd(20, "c_nngetsloped", filee, emsg);
     *ier = 20;
     *slope = -999.;
     return;
   }
   if (col >= curas.ccols || col < 0) 
   {
     sprintf(emsg,"\n  Requested column = %d (indices starting with one)\n",
               col+1);
     ErrorHnd(21, "c_nngetsloped", filee, emsg);
     *ier = 21;
     *slope = -999.;
     return;
   }
   *ier = 0;
   *slope = (curas.slope_out)[row][col];
}
void c_nngetaspectd(int row, int col, double *aspect, int *ier)
{
   if (asflag == 0) {
     error_status = 28;
     ErrorHnd(error_status, "c_nngetaspectd", filee, "\n");
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
     ErrorHnd(25, "c_nngetaspectd", filee, emsg);
     *ier = 25;
     *aspect = -999.;
     return;
   }
   if (curas.crows == 0)
   {
     ErrorHnd(22, "c_nngetaspectd", filee, "\n");
     *ier = 22;
     *aspect = -999.;
     return;
   }
   if (row >= curas.crows || row < 0)
   {
     sprintf(emsg,"\n  Requested row = %d (indices starting with one)\n",row+1);
     ErrorHnd(20, "c_nngetaspectd", filee, emsg);
     *ier = 20;
     *aspect = -999.;
     return;
   }
   if (col >= curas.ccols || col < 0)
   {
     sprintf(emsg,"\n  Requested column = %d (indices starting with one)\n",
               col);
     ErrorHnd(21, "c_nngetaspectd", filee, emsg);
     *ier = 21;
     *aspect = -999.;
     return;
   }
   *ier = 0;
   *aspect = (curas.aspect_out)[row][col];
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

   if (ReadData(n,x,y,z) != 0) 
   {
      ErrorHnd(error_status, "c_nnpntinitd", filee, "\n");
   }
}
void c_nnpntd(double x, double y, double *point)
{
   int   idum, nxi=3, nyi=3, ierr;
   double xdum[1], ydum[1], zdum[1], xi[3], yi[3], **out;

/*
 *  Check to see if the input point is within the gridded region
 *  set up in the initialization.
 */
   if ( (x < xstart) || (x > xend) || (y < ystart) || (y > yend) )
   {
      sprintf(emsg,"\n  Coordinate = (%f, %f)\n", x, y);
      ErrorHnd(27, "c_nnpntd", filee, emsg);
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
      ErrorHnd(28, "c_nnpnts", filee, "\n");
      error_status = ierr;
      *point = -999.;
   }
   
   *point = out[1][1];
}
void c_nnpntendd()
{
   single_point = 0;
   first_single = 0;
   horilap = horilap_save;
   vertlap = vertlap_save;
   Terminate();
}
