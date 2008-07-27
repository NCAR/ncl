/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ngmath.h>
#include "dstypes.h"
#include "dsproto.h"
#include "dsgvars.h"

/*
 *  Given three points in three space a, b, c, find the angle
 *  between the vector from b to a and the vector from b to c.
 */
double dsangd (DSpointd3 a, DSpointd3 b, DSpointd3 c)
{
  DSpointd3 vector_1,vector_2;
  double    cosd;

  vector_1.x = a.x - b.x;
  vector_1.y = a.y - b.y;
  vector_1.z = a.z - b.z;
  
  vector_2.x = c.x - b.x;
  vector_2.y = c.y - b.y;
  vector_2.z = c.z - b.z;

  cosd = dotd(vector_1,vector_2)/(magd(vector_1)*magd(vector_2));
  if (cosd >  1.) cosd =  1.; 
  if (cosd < -1.) cosd = -1.; 

  return(acos(cosd));
}

/*
 *  Find the magnitude of a vector.
 */
double magd(DSpointd3 p)
{
  return(sqrt(p.x*p.x + p.y*p.y + p.z*p.z));
}

/*
 *  Find the dot product of two vectors.
 */
double dotd(DSpointd3 p, DSpointd3 q)
{
  return(p.x*q.x + p.y*q.y + p.z*q.z);
}

/*
 *  Sort a linear array ar in place in ascending order and
 *  sort a companion integer array in the same order.
 */
void dssortd(int n, double a[], int ip[])
{
  double v;
  int i, j, h, iv;

  for (h = 1; h <= n/9; h = 3*h+1);

  for ( ; h > 0; h /= 3) {
    for (i = h; i < n; i ++) {
      v = a[i];
      iv = ip[i];
      j = i;
      while (j > h-1 && a[j-h] > v) {
        a[j] = a[j-h];
        ip[j] = ip[j-h];
        j -= h;
      }
      a[j] = v;
      ip[j] = iv;
    }
  }
}

/*
 *  Computes distances between a set of input points and
 *  a given point.  The distances are returned in a sorted array.
 */
void dsdist(int n, DSpointd3 p[], DSpointd3 q, double *sdist)
/*
 *        n - The number of points in the p array.
 *        p - An array of n points.
 *        q - An individual point.
 *    sdist - A pointer to an array of n distances to point q.
 */
{
  DSpointd3 dp;
  int       i;
  double    dtmp;

  for (i = 0; i < n; i++) {
    dp.x = p[i].x - q.x; 
    dp.y = p[i].y - q.y; 
    dp.z = p[i].z - q.z; 
    dtmp = magd(dp);
    if (dtmp <= ds_max_dist * ds_scale) {
      sdist[i] = dtmp;
    }
    else {
      sdist[i] = -1.;
    }  
  }
}
