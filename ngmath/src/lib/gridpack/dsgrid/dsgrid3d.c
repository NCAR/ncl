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
 */
double *c_dsgrid3d(int n, double x[], double y[], double z[], double u[],
                  int nx, int ny, int nz, double xo[], double yo[], 
                  double zo[], int *ier)
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
  static    double perror = 1.;
  double    xc, yc, zc, *retval;

  DSpointd3 q;

/*
 *  Get memory and do initialization.
 */
  *ier = 0;

  dsgetmem(n, nx, ny, nz, ier);
  if (*ier != 0) return(&perror);

  dsinit(n, nx, ny, nz, x, y, z, xo, yo, zo, ier);
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
 *  Compute the distances from (xc, yc, zc) to all the input points.
 */
        q.x = xc * ds_scale;
        q.y = yc * ds_scale;
        q.z = zc * ds_scale;
        dsdist(n, ds_input_points, q, ds_distances);
  
/*
 *  Calculate the interpolated value.
 */
        if (ds_shadowing) {
          ds_output[nz*ny*i + nz*j + k] = 
                          svalue(n, u, q.x, q.y, q.z);
        } 
        else {
          ds_output[nz*ny*i + nz*j + k] = 
                          ivalue(n, u, q.x, q.y, q.z);
        }
      }
    }
  }
  
  retval = &ds_output[0];
  dsfreemem();
  ds_first_call = 0;
  return(retval);
}

/*
 *  Get required memory.
 */
void dsgetmem(int n, int nx, int ny, int nz, int *ier)
{
  ds_input_points       = (DSpointd3 *) calloc(n, sizeof(DSpointd3));
  if (ds_input_points == NULL) {
    DSErrorHnd(6, "dsgetmem", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_distances             = (double *) calloc(n, sizeof(double));
  if (ds_distances == NULL) {
    DSErrorHnd(9, "dsgetmem", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_weights               = (double *) calloc(n, sizeof(double));
  if (ds_weights == NULL) {
    DSErrorHnd(10, "dsgetmem", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_permutation_vector = (int *) calloc(n, sizeof(int));
  if (ds_permutation_vector == NULL) {
    DSErrorHnd(11, "dsgetmem", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
  ds_output             = (double *) calloc(nx*ny*nz, sizeof(double));
  if (ds_output == NULL) {
    DSErrorHnd(12, "dsgetmem", stderr, "\n");
    *ier = ds_error_status;
    return;
  }
}

/*
 *  Free memory.
 */
void dsfreemem()
{
  free(ds_input_points);
  free(ds_distances);
  free(ds_weights);
  free(ds_permutation_vector);
}

/*
 *  Initialization.
 */
void dsinit(int n, int nx, int ny, int nz, double x[], double y[], double z[], 
            double xo[], double yo[], double zo[], int *ier)
{
  int    i;
  double xmn, ymn, zmn, xmx, ymx, zmx, tlm;

  if (ds_set_maxpts == 0) ds_maxpoints  = n;

  if (n < 3) {
    DSErrorHnd(2, "dsinit", stderr, "\n");
    *ier = ds_error_status;
    return;
  }

  if ((nx <= 0) || (ny <=0) || (nz <= 0)) {
    DSErrorHnd(3, "dsinit", stderr, "\n");
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
  ds_scale = 1./tlm;
  for (i = 0; i < n; i++) {
    ds_input_points[i].x = x[i] * ds_scale; 
    ds_input_points[i].y = y[i] * ds_scale; 
    ds_input_points[i].z = z[i] * ds_scale; 
  }

  ds_epsilon_test = 1.E-5;

  for (i = 0; i < nx*ny*nz; i++) {
    ds_output[i] = ds_missing_value;
  }
}

/*
 *  Calculate interpolated value when the shadowing option is
 *  turned off.
 */
double ivalue(int num_points, double *values, 
                   double xc, double yc, double zc)
{
  int       iw, it;
  double    normalization_factor, interpolated_value, weight_sum;

  for (iw = 0; iw < num_points; iw++) {
    if ( (ds_distances[iw] < ds_epsilon_test) && (ds_distances[iw] >= 0.) ) {
      for (it = 0; it < num_points; it++) {
        ds_weights[it] = 0.;
      }
      ds_weights[iw] = 1.;
      goto label_1;
    }
  }

  dweights(num_points);
  
  for (weight_sum = 0., iw = 0; iw < num_points; iw++) {
    weight_sum += ds_weights[iw];
  }

  if (weight_sum == 0.) {
    DSErrorHnd(14, "ivalue", stderr, "\n");    
    return(ds_missing_value);
  }
  normalization_factor = 1./weight_sum;
  for (iw = 0; iw < num_points; iw++) {
    ds_weights[iw] *= normalization_factor;
  }
 
label_1:
  for (interpolated_value = 0., iw = 0; iw < num_points; iw++) {
    interpolated_value += values[iw] * ds_weights[iw];
  }

  return(interpolated_value);
  
}

/*
 *  Calculate interpolated value when shadowing option is on.
 */
double svalue(int num_points, double *values, 
                   double xc, double yc, double zc)
{
  int       iw, ia, it, lp;
  double    normalization_factor, interpolated_value, weight_sum;

  for (iw = 0; iw < num_points; iw++) {
    if ( (ds_distances[iw] < ds_epsilon_test) && (ds_distances[iw] >= 0.) ) {
      for (it = 0; it < num_points; it++) {
        ds_weights[it] = 0.;
      }
      ds_weights[iw] = 1.;
      goto label_1;
    }
  }

  for (it = 0; it < num_points; it++) {
    ds_permutation_vector[it] = it;
  }
  dssortd(num_points, ds_distances, ds_permutation_vector);
  sweights(num_points, xc, yc, zc);
  
  for (weight_sum = 0., iw = 0; iw < num_points; iw++) {
    weight_sum += ds_weights[iw];
  }

  if (weight_sum == 0.) {
    DSErrorHnd(14, "svalue", stderr, "\n");
    return(ds_missing_value);
  }
  normalization_factor = 1./weight_sum;
  for (iw = 0; iw < num_points; iw++) {
    ds_weights[iw] *= normalization_factor;
  }
 
label_1:
  for (interpolated_value = 0., iw = 0; iw < num_points; iw++) {
    lp = ds_permutation_vector[iw];
    interpolated_value += values[iw] * ds_weights[iw];
  }
  return(interpolated_value);
  
}

/*
 *  3D interpolation in point mode.
 */
void c_dspnt3d(int n, double xi[], double yi[], double zi[], double ui[],
               int m, double xo[], double yo[], double zo[], double uo[],
               int *ier)
{
  int    i;
  double xt[1], yt[1], zt[1];

  for (i = 0; i < m; i++) {
    xt[0] = xo[i];
    yt[0] = yo[i];
    zt[0] = zo[i];
    uo[i] = *c_dsgrid3d(n, xi, yi, zi, ui, 1, 1, 1, xt, yt, zt, ier);
  }
}

/*
 * Claculate weights when shadowing is on.
 */
void sweights(int n, double xc, double yc, double zc)
{
  int       lpw, lpa, iw, ia;
  double    minimum_angle, angle, shadow_scale;
  DSpointd3 p1, p2, p3;

  lpw = ds_permutation_vector[0];
  if (ds_distances[lpw] > 0.) {
    ds_weights[lpw] = 1./dist_pow(ds_distances[0]);
  }
  else {
    ds_weights[lpw] = 0.;
  }

  for (iw = 1; iw < n; iw++) {
    minimum_angle = PIH;
    lpw = ds_permutation_vector[n-iw];
    p1.x = ds_input_points[lpw].x;
    p1.y = ds_input_points[lpw].y;
    p1.z = ds_input_points[lpw].z;
    p2.x = xc;
    p2.y = yc;
    p2.z = zc;
    for (ia = 0; ia < n - iw; ia++) {
      lpa = ds_permutation_vector[ia];
      p3.x = ds_input_points[lpa].x;
      p3.y = ds_input_points[lpa].y;
      p3.z = ds_input_points[lpa].z;
      angle = dsangd(p1,p2,p3);
      if (angle < minimum_angle) minimum_angle = angle;
    }
    shadow_scale = tan(0.5*minimum_angle);

    if (ds_distances[n-iw] > 0.) {
      ds_weights[lpw] = shadow_scale / dist_pow(ds_distances[n-iw]);
    }
    else {
      ds_weights[lpw] = 0.;
    }
  }
}

void dweights(int n)
{
  int it;

  for (it = 0; it < n; it++) {
    if (ds_distances[it] > 0.) {
      ds_weights[it] = 1./ dist_pow(ds_distances[it]);
    }
    else {
      ds_weights[it] = 0.;
    }
  }
}

/*
 *  Calculate powers of the distances.
 */
double dist_pow(double dist)
{
  double dtmp;

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
  if (dtmp < (double) 1.E-30) {
    return ( (double) 1.E-30);
  }
  else {
    return (dtmp);
  }
}
