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
#include "dsuhead.h"

#define PIH 1.5707963

/*
 *  Interpolate randomly-spaced 3D input data to a regularly spaced grid.
 *  Single precision version.
 */
float *c_dsgrid3s(int n, float x[], float y[], float z[], float u[],
                  int nx, int ny, int nz, float xo[], float yo[], 
                  float zo[], int *ier)
/*
 *    Arguments
 *    ---------
 *        n - The number of input data points.
 *        x - An array of X coordinate values of the input data points.
 *        y - An array of Y coordinate values of the input data points.
 *        z - An array of Z coordinate values of the input data points.
 *        u - The functional value at coordinate (x,y,z).
 *       nx - The dimension of the array xo containing the X coordinate 
 *            values for the output grid.
 *       ny - The dimension of the array yo containing the Y coordinate 
 *            values for the output grid.
 *       nz - The dimension of the array zo containing the z coordinate 
 *            values for the output grid.
 *       xo - The array containing the X coordinate values for the output 
 *            grid (must be monotone increasing, but need not be equally 
 *            spaced.
 *       yo - The array containing the Y coordinate values for the output 
 *            grid (must be monotone increasing, but need not be equally 
 *            spaced.
 *       zo - The array containing the Z coordinate values for the output 
 *            grid (must be monotone increasing, but need not be equally 
 *            spaced.
 *     *ier - An error return value ( = 0 is no error).
 *
 *   Return value
 *   ------------
 *      A pointer to the first element of a linear array that is
 *      laid out as a 3D array (i.e. the last subscript varies fastest)
 *      of dimension: nx by ny by nz.
 */
{
  int       i, j, k;
  static    float perror = 1.;
  float     xc, yc, zc, *retval;

  DSpoints3 q;

/*
 *  Get memory and do initialization.
 */
  *ier = 0;

  dsgetmem_s(n, nx, ny, nz, ier);
  if (*ier != 0) return(&perror);

  dsinit_s(n, nx, ny, nz, x, y, z, xo, yo, zo, ier);
  if (*ier != 0) return(&perror);
    
/*
 *  Loop over the output grid
 */
  for (i = 0; i < nx; i++) {
    xc = xo[i]; 
    for (j = 0; j < ny; j++) {
      yc = yo[j]; 
      for (k = 0; k < nz; k++) {
        zc = zo[k]; 

/*
 *  Compute the distances from (xc, yc, zc) to all the input points 
 *  and order these distances.
 */
        q.x = xc * (float) ds_scale;
        q.y = yc * (float) ds_scale;
        q.z = zc * (float) ds_scale;

        dsdist_s(n, ds_input_points_s, q, ds_distances_s);

        if (ds_shadowing) {
          ds_output_s[nz*ny*i + nz*j + k] = 
                          svalue_s(n, u, xc, yc, zc);
        } 
        else {
          ds_output_s[nz*ny*i + nz*j + k] = 
                          ivalue_s(n, u, xc, yc, zc);
        }
      }
    }
  }
  
  retval = &ds_output_s[0];
  dsfreemem_s();
  ds_first_call = 0;
  return(retval);
}

/*
 *  Get required memory.
 */
void dsgetmem_s(int n, int nx, int ny, int nz, int *ier)
{
  ds_input_points_s     = (DSpoints3 *) calloc(n, sizeof(DSpoints3));
  if (ds_input_points_s == NULL) {
    DSErrorHnd(6, "dsgetmem_s", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_distances_s        = (float *) calloc(n, sizeof(float));
  if (ds_distances_s == NULL) {
    DSErrorHnd(9, "dsgetmem_s", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_weights_s          = (float *) calloc(n, sizeof(float));
  if (ds_weights_s == NULL) {
    DSErrorHnd(10, "dsgetmem", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_permutation_vector = (int *) calloc(n, sizeof(int));
  if (ds_permutation_vector == NULL) {
    DSErrorHnd(11, "dsgetmem_s", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_output_s           = (float *) calloc(nx*ny*nz, sizeof(float));
  if (ds_output_s == NULL) {
    DSErrorHnd(12, "dsgetmem_s", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
}

/*
 *  Free memory.
 */
void dsfreemem_s()
{
  free(ds_input_points_s);
  free(ds_distances_s);
  free(ds_weights_s);
  free(ds_permutation_vector);
}

/*
 *  Initialization.
 */
void dsinit_s(int n, int nx, int ny, int nz, float x[], float y[], float z[], 
              float xo[], float yo[], float zo[], int *ier)
{
  int    i;
  float  xmn, ymn, zmn, xmx, ymx, zmx, tlm;

  if (ds_set_maxpts == 0) ds_maxpoints = n;

  if (n < 3) {
    DSErrorHnd(2, "dsinit_s", stderr, "\n");
    *ier = ds_error_status;
    return;
  }

  if ((nx <= 0) || (ny <=0) || (nz <= 0)) {
    DSErrorHnd(3, "dsinit_s", stderr, "\n");
    *ier = ds_error_status;
    return;
  }

  xmn = xo[0];
  ymn = yo[0];
  zmn = zo[0];
  xmx = xo[0];
  ymx = yo[0];
  zmx = zo[0];
  for (i = 0; i < nx; i++) {
    xmn = MIN(xo[i], xmn);
    xmx = MAX(xo[i], xmx);
  }
  for (i = 0; i < ny; i++) {
    ymn = MIN(yo[i], ymn);
    ymx = MAX(yo[i], ymx);
  }
  for (i = 0; i < nz; i++) {
    zmn = MIN(zo[i], zmn);
    zmx = MAX(zo[i], zmx);
  }
  for (i = 0; i < n; i++) {
    xmn = MIN(x[i], xmn);
    xmx = MAX(x[i], xmx);
    ymn = MIN(y[i], ymn);
    ymx = MAX(y[i], ymx);
    zmn = MIN(z[i], zmn);
    zmx = MAX(z[i], zmx);
  }

/*
 *  Find the maximum span of the three coordinates.
 */
  tlm = MAX(MAX((xmx - xmn), ymx - ymn), zmx - zmn);

/*
 *  Set the maximum distance for point inclusion to include all
 *  points, if this has not been specifically set by the user.
 */
  if (ds_set_max_dist == 0) ds_max_dist = 
            2.*((xmx - xmn) + (ymx - ymn) + (zmx - zmn));

/*
 *  Scale and store the input values.
 */
  ds_scale = (double) (1./tlm);
  for (i = 0; i < n; i++) {
    ds_input_points_s[i].x = x[i] * (float) ds_scale;
    ds_input_points_s[i].y = y[i] * (float) ds_scale;
    ds_input_points_s[i].z = z[i] * (float) ds_scale;
  }

  ds_epsilon_test = 1.E-5;

  for (i = 0; i < nx*ny*nz; i++) {
    ds_output_s[i] = (float) ds_missing_value;
  }
}

/*
 *  Calculate interpolated value when the shadowing option is
 *  turned off.
 */
float ivalue_s(int num_points, float *values, 
                   float xc, float yc, float zc)
{
  int       iw, it;
  float     normalization_factor, interpolated_value, weight_sum;

  for (iw = 0; iw < num_points; iw++) {
    if ( (ds_distances_s[iw] < ds_epsilon_test) && (ds_distances_s[iw] >= 0.) ) {
      for (it = 0; it < num_points; it++) {
        ds_weights_s[it] = 0.;
      }
      ds_weights_s[iw] = 1.;
      goto label_1;
    }
  }

  dweights_s(num_points);
  
  for (weight_sum = 0., iw = 0; iw < num_points; iw++) {
    weight_sum += ds_weights_s[iw];
  }
  if (weight_sum == 0.) {
    DSErrorHnd(14, "ivalue_s", stderr, "\n");
    return( (float) ds_missing_value);
  }
  normalization_factor = 1./weight_sum;
  for (iw = 0; iw < num_points; iw++) {
    ds_weights_s[iw] *= normalization_factor;
  }
 
label_1:
  for (interpolated_value = 0., iw = 0; iw < num_points; iw++) {
    interpolated_value += values[iw] * ds_weights_s[iw];
  }
  return(interpolated_value);
  
}

/*
 *  Calculate interpolated value when shadowing option is on.
 */
float svalue_s(int num_points, float *values, 
                   float xc, float yc, float zc)
{
  int       iw, ia, it, lp;
  float     normalization_factor, interpolated_value, weight_sum;

  for (iw = 0; iw < num_points; iw++) {
    if ((ds_distances_s[iw] < ds_epsilon_test) && (ds_distances_s[iw] >= 0.)) {
      for (it = 0; it < num_points; it++) {
        ds_weights_s[it] = 0.;
      }
      ds_weights_s[iw] = 1.;
      goto label_1;
    }
  }

  for (it = 0; it < num_points; it++) {
    ds_permutation_vector[it] = it;
  }
  dssorts(num_points, ds_distances_s, ds_permutation_vector);
  sweights_s(num_points, xc, yc, zc);
  
  for (weight_sum = 0., iw = 0; iw < num_points; iw++) {
    weight_sum += ds_weights_s[iw];
  }
  if (weight_sum == 0.) {
    DSErrorHnd(14, "svalue_s", stderr, "\n");
    return( (float) ds_missing_value);
  }
  normalization_factor = 1./weight_sum;
  for (iw = 0; iw < num_points; iw++) {
    ds_weights_s[iw] *= normalization_factor;
  }
 
label_1:
  for (interpolated_value = 0., iw = 0; iw < num_points; iw++) {
    lp = ds_permutation_vector[iw];
    interpolated_value += values[iw] * ds_weights_s[iw];
  }
  return(interpolated_value);
  
}

/*
 *  3D interpolation in point mode.
 */
void c_dspnt3s(int n, float xi[], float yi[], float zi[], float ui[],
               int m, float xo[], float yo[], float zo[], float uo[],
               int *ier)
{
  int    i;
  float  xt[1], yt[1], zt[1];

  for (i = 0; i < m; i++) {
    xt[0] = xo[i];
    yt[0] = yo[i];
    zt[0] = zo[i];
    uo[i] = *c_dsgrid3s(n, xi, yi, zi, ui, 1, 1, 1, xt, yt, zt, ier);
  }
}

/*
 * Claculate weights when shadowing is on.
 */
void sweights_s(int n, float xc, float yc, float zc)
{
  int       lpw, lpa, iw, ia;
  float     minimum_angle, angle, shadow_scale;
  DSpoints3 p1, p2, p3;

  lpw = ds_permutation_vector[0];
  if (ds_distances_s[lpw] > 0.) {
    ds_weights_s[lpw] = 1./ dist_pow_s(ds_distances_s[0]);
  }
  else {
    ds_weights_s[lpw] = 0.;
  }

  for (iw = 1; iw < n; iw++) {
    minimum_angle = PIH;
    lpw = ds_permutation_vector[n-iw];
    p1.x = ds_input_points_s[lpw].x;
    p1.y = ds_input_points_s[lpw].y;
    p1.z = ds_input_points_s[lpw].z;
    p2.x = xc;
    p2.y = yc;
    p2.z = zc;
    for (ia = 0; ia < n - iw; ia++) {
      lpa = ds_permutation_vector[ia];
      p3.x = ds_input_points_s[lpa].x;
      p3.y = ds_input_points_s[lpa].y;
      p3.z = ds_input_points_s[lpa].z;
      angle = dsangs(p1,p2,p3);
      if (angle < minimum_angle) minimum_angle = angle;
    }
    shadow_scale = tan(0.5*minimum_angle);

    if (ds_distances_s[n-iw] > 0.) {
      ds_weights_s[lpw] = shadow_scale / dist_pow_s(ds_distances_s[n-iw]);
    }
    else {
      ds_weights_s[lpw] = 0.;
    }

  }
}

void dweights_s(int n)
{
  int it;

  for (it = 0; it < n; it++) {
    if (ds_distances_s[it] > 0.) {
      ds_weights_s[it] = 1./ dist_pow_s(ds_distances_s[it]);
    }
    else {
      ds_weights_s[it] = 0.;
    }
  }
}

/*
 *  Calculate powers of the distances.
 */
float dist_pow_s(float dist)
{
  float dtmp;

  if (ds_exponent == 3.0) {
    dtmp = dist*dist*dist;
  }
  else if (ds_exponent == 1.0) {
    dtmp = dist;
  }
  else if (ds_exponent == 0.5) {
    dtmp = sqrt(dist);
  }
  else if (ds_exponent == 2.0) {
    dtmp = dist*dist;
  }
  else if (ds_exponent == 4.0) {
    dtmp = dist*dist;
    dtmp = dtmp*dtmp;
  }
  else if (ds_exponent == 5.0) {
    dtmp = dist*dist*dist*dist*dist;
  }
  else if (ds_exponent == 6.0) {
    dtmp = dist*dist*dist;
    dtmp = dtmp*dtmp;
  }
  else if (ds_exponent == 7.0) {
    dtmp = dist*dist*dist*dist*dist*dist*dist;
  }
  else if (ds_exponent == 8.0) {
    dtmp = dist*dist;
    dtmp = dtmp*dtmp;
    dtmp = dtmp*dtmp;
  }
  else if (ds_exponent == 9.0) {
    dtmp = dist*dist*dist;
    dtmp = dtmp*dtmp*dtmp;
  }
  else if (ds_exponent == 10.0) {
    dtmp = dist*dist*dist*dist*dist;
    dtmp = dtmp*dtmp;
  }
  else {
    dtmp = pow(dist, ds_exponent);
  }
  if (dtmp < 1.E-30) {
    return (1.E-30);
  }
  else {
    return (dtmp);
  }
}
