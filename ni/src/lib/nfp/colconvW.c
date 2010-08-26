#include "wrapper.h"

extern void NGCALLF(drgbhsv,DRGBHSV)(double*,double*,double*,
                                     double*,double*,double*);
extern void NGCALLF(dhsvrgb,DHSVRGB)(double*,double*,double*,
                                     double*,double*,double*);
extern void NGCALLF(drgbhls,DRGBHLS)(double*,double*,double*,
                                     double*,double*,double*);
extern void NGCALLF(dhlsrgb,DHLSRGB)(double*,double*,double*,
                                     double*,double*,double*);
extern void NGCALLF(drgbyiq,DRGBYIQ)(double*,double*,double*,
                                     double*,double*,double*);
extern void NGCALLF(dyiqrgb,DYIQRGB)(double*,double*,double*,
                                     double*,double*,double*);

NhlErrorTypes rgbhsv_W( void )
{
/*
 * Input variables
 */
  void *rgb;
  double *drgb;
  ng_size_t dsizes_rgb[NCL_MAX_DIMENSIONS];
  int ndims_rgb;
  NclBasicDataTypes type_rgb;
 
/*
 * output variable 
 */
  float *fhsv = NULL;
  double *dhsv = NULL;
  ng_size_t size_hsv;
  NclBasicDataTypes type_hsv;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,j;
  double *thsv;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  rgb = (void*)NclGetArgValue(
          0,
          1,
          &ndims_rgb,
          dsizes_rgb,
          NULL,
          NULL,
          &type_rgb,
          DONT_CARE);


/*
 * Check dimension sizes. The argument must be an array with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for rgb.
 */
  if(dsizes_rgb[ndims_rgb-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhsv: the last (rightmost) dimension of the input array must be of size 3");
    return(NhlFATAL);
  }

/*
 * Determine size for the return array.
 */
  size_hsv = 1;
  for (i = 0; i < ndims_rgb-1; i++) {
    size_hsv *= dsizes_rgb[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  drgb  = coerce_input_double(rgb, type_rgb, 3*size_hsv, 0, NULL, NULL);
  if(drgb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhsv: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_hsv = NCL_float;
  if (type_rgb == NCL_double) {
    type_hsv = NCL_double;
    dhsv = (double *)calloc(3*size_hsv, sizeof(double));
    if(dhsv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhsv: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    fhsv = (float *)calloc(3*size_hsv, sizeof(float));
    if(fhsv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhsv: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  thsv = (double *) calloc(3,sizeof(double));
  for( i = 0; i < size_hsv; i++ ) {
    NGCALLF(drgbhsv,DRGBHSV)(drgb+3*i, drgb+3*i+1, drgb+3*i+2, 
                             thsv, thsv+1,thsv+2);
    for (j = 0; j < 3; j++) {
      if (type_hsv == NCL_double) {
        dhsv[3*i+j] = thsv[j];
      }
      else {
        fhsv[3*i+j] = (float)thsv[j];
      }
    } 
  } 

/*
 * free memory.
 */
  if((void*)drgb != rgb) NclFree(drgb);
  NclFree(thsv);

/*
 * Return.
 */
  if (type_hsv == NCL_double) {
    return(NclReturnValue(dhsv,ndims_rgb, dsizes_rgb, NULL, type_hsv, 0));
  }
  else {
    return(NclReturnValue(fhsv,ndims_rgb, dsizes_rgb, NULL, type_hsv, 0));
  }
}

NhlErrorTypes hsvrgb_W( void )
{
/*
 * Input variables
 */
  void *hsv;
  double *dhsv;
  ng_size_t dsizes_hsv[NCL_MAX_DIMENSIONS];
  int ndims_hsv;
  NclBasicDataTypes type_hsv;
 
/*
 * output variable 
 */
  float *frgb = NULL;
  double *drgb = NULL;
  ng_size_t size_rgb;
  NclBasicDataTypes type_rgb;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,j;
  double *trgb;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  hsv = (void*)NclGetArgValue(
          0,
          1,
          &ndims_hsv,
          dsizes_hsv,
          NULL,
          NULL,
          &type_hsv,
          DONT_CARE);


/*
 * Check dimension sizes. The argument must be an array with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for hsv.
 */
  if(dsizes_hsv[ndims_hsv-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hsvrgb: the last (rightmost) dimension of the input array must be of size 3");
    return(NhlFATAL);
  }

/*
 * Determine size for the return array.
 */
  size_rgb = 1;
  for (i = 0; i < ndims_hsv-1; i++) {
    size_rgb *= dsizes_hsv[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dhsv  = coerce_input_double(hsv, type_hsv, 3*size_rgb, 0, NULL, NULL);
  if(dhsv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hsvrgb: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_rgb = NCL_float;
  if (type_hsv == NCL_double) {
    type_rgb = NCL_double;
    drgb = (double *)calloc(3*size_rgb, sizeof(double));
    if(drgb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hsvrgb: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    frgb = (float *)calloc(3*size_rgb, sizeof(float));
    if(frgb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hsvrgb: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  trgb = (double *) calloc(3,sizeof(double));
  for( i = 0; i < size_rgb; i++ ) {
    NGCALLF(dhsvrgb,DHSVRGB)(dhsv+3*i, dhsv+3*i+1, dhsv+3*i+2, 
                             trgb, trgb+1,trgb+2);
    for (j = 0; j < 3; j++) {
      if (type_rgb == NCL_double) {
        drgb[3*i+j] = trgb[j];
      }
      else {
        frgb[3*i+j] = (float)trgb[j];
      }
    } 
  } 

/*
 * free memory.
 */
  if((void*)dhsv != hsv) NclFree(dhsv);
  NclFree(trgb);

/*
 * Return.
 */
  if (type_rgb == NCL_double) {
    return(NclReturnValue(drgb,ndims_hsv, dsizes_hsv, NULL, type_rgb, 0));
  }
  else {
    return(NclReturnValue(frgb,ndims_hsv, dsizes_hsv, NULL, type_rgb, 0));
  }
}

NhlErrorTypes rgbhls_W( void )
{
/*
 * Input variables
 */
  void *rgb;
  double *drgb;
  ng_size_t dsizes_rgb[NCL_MAX_DIMENSIONS];
  int ndims_rgb;
  NclBasicDataTypes type_rgb;
 
/*
 * output variable 
 */
  float *fhls = NULL;
  double *dhls = NULL;
  ng_size_t size_hls;
  NclBasicDataTypes type_hls;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,j;
  double *thls;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  rgb = (void*)NclGetArgValue(
          0,
          1,
          &ndims_rgb,
          dsizes_rgb,
          NULL,
          NULL,
          &type_rgb,
          DONT_CARE);


/*
 * Check dimension sizes. The argument must be an array with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for rgb.
 */
  if(dsizes_rgb[ndims_rgb-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhls: the last (rightmost) dimension of the input array must be of size 3");
    return(NhlFATAL);
  }

/*
 * Determine size for the return array.
 */
  size_hls = 1;
  for (i = 0; i < ndims_rgb-1; i++) {
    size_hls *= dsizes_rgb[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  drgb  = coerce_input_double(rgb, type_rgb, 3*size_hls, 0, NULL, NULL);
  if(drgb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhls: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_hls = NCL_float;
  if (type_rgb == NCL_double) {
    type_hls = NCL_double;
    dhls = (double *)calloc(3*size_hls, sizeof(double));
    if(dhls == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhls: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    fhls = (float *)calloc(3*size_hls, sizeof(float));
    if(fhls == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbhls: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  thls = (double *) calloc(3,sizeof(double));
  for( i = 0; i < size_hls; i++ ) {
    NGCALLF(drgbhls,DRGBHLS)(drgb+3*i, drgb+3*i+1, drgb+3*i+2, 
                             thls, thls+1,thls+2);
    for (j = 0; j < 3; j++) {
      if (type_hls == NCL_double) {
        dhls[3*i+j] = thls[j];
      }
      else {
        fhls[3*i+j] = (float)thls[j];
      }
    } 
  } 

/*
 * free memory.
 */
  if((void*)drgb != rgb) NclFree(drgb);
  NclFree(thls);

/*
 * Return.
 */
  if (type_hls == NCL_double) {
    return(NclReturnValue(dhls,ndims_rgb, dsizes_rgb, NULL, type_hls, 0));
  }
  else {
    return(NclReturnValue(fhls,ndims_rgb, dsizes_rgb, NULL, type_hls, 0));
  }
}

NhlErrorTypes hlsrgb_W( void )
{
/*
 * Input variables
 */
  void *hls;
  double *dhls;
  ng_size_t dsizes_hls[NCL_MAX_DIMENSIONS];
  int ndims_hls;
  NclBasicDataTypes type_hls;
 
/*
 * output variable 
 */
  float *frgb = NULL;
  double *drgb = NULL;
  ng_size_t size_rgb;
  NclBasicDataTypes type_rgb;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,j;
  double *trgb;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  hls = (void*)NclGetArgValue(
          0,
          1,
          &ndims_hls,
          dsizes_hls,
          NULL,
          NULL,
          &type_hls,
          DONT_CARE);


/*
 * Check dimension sizes. The argument must be an array with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for hls.
 */
  if(dsizes_hls[ndims_hls-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hlsrgb: the last (rightmost) dimension of the input array must be of size 3");
    return(NhlFATAL);
  }

/*
 * Determine size for the return array.
 */
  size_rgb = 1;
  for (i = 0; i < ndims_hls-1; i++) {
    size_rgb *= dsizes_hls[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dhls  = coerce_input_double(hls, type_hls, 3*size_rgb, 0, NULL, NULL);
  if(dhls == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hlsrgb: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_rgb = NCL_float;
  if (type_hls == NCL_double) {
    type_rgb = NCL_double;
    drgb = (double *)calloc(3*size_rgb, sizeof(double));
    if(drgb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hlsrgb: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    frgb = (float *)calloc(3*size_rgb, sizeof(float));
    if(frgb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hlsrgb: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  trgb = (double *) calloc(3,sizeof(double));
  for( i = 0; i < size_rgb; i++ ) {
    NGCALLF(dhlsrgb,DHLSRGB)(dhls+3*i, dhls+3*i+1, dhls+3*i+2, 
                             trgb, trgb+1,trgb+2);
    for (j = 0; j < 3; j++) {
      if (type_rgb == NCL_double) {
        drgb[3*i+j] = trgb[j];
      }
      else {
        frgb[3*i+j] = (float)trgb[j];
      }
    } 
  } 

/*
 * free memory.
 */
  if((void*)dhls != hls) NclFree(dhls);
  NclFree(trgb);

/*
 * Return.
 */
  if (type_rgb == NCL_double) {
    return(NclReturnValue(drgb,ndims_hls, dsizes_hls, NULL, type_rgb, 0));
  }
  else {
    return(NclReturnValue(frgb,ndims_hls, dsizes_hls, NULL, type_rgb, 0));
  }
}

NhlErrorTypes rgbyiq_W( void )
{
/*
 * Input variables
 */
  void *rgb;
  double *drgb;
  ng_size_t dsizes_rgb[NCL_MAX_DIMENSIONS];
  int ndims_rgb;
  NclBasicDataTypes type_rgb;
 
/*
 * output variable 
 */
  float *fyiq = NULL;
  double *dyiq = NULL;
  ng_size_t size_yiq;
  NclBasicDataTypes type_yiq;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,j;
  double *tyiq;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  rgb = (void*)NclGetArgValue(
          0,
          1,
          &ndims_rgb,
          dsizes_rgb,
          NULL,
          NULL,
          &type_rgb,
          DONT_CARE);


/*
 * Check dimension sizes. The argument must be an array with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for rgb.
 */
  if(dsizes_rgb[ndims_rgb-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbyiq: the last (rightmost) dimension of the input array must be of size 3");
    return(NhlFATAL);
  }

/*
 * Determine size for the return array.
 */
  size_yiq = 1;
  for (i = 0; i < ndims_rgb-1; i++) {
    size_yiq *= dsizes_rgb[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  drgb  = coerce_input_double(rgb, type_rgb, 3*size_yiq, 0, NULL, NULL);
  if(drgb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbyiq: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_yiq = NCL_float;
  if (type_rgb == NCL_double) {
    type_yiq = NCL_double;
    dyiq = (double *)calloc(3*size_yiq, sizeof(double));
    if(dyiq == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbyiq: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    fyiq = (float *)calloc(3*size_yiq, sizeof(float));
    if(fyiq == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgbyiq: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  tyiq = (double *) calloc(3,sizeof(double));
  for( i = 0; i < size_yiq; i++ ) {
    NGCALLF(drgbyiq,DRGBYIQ)(drgb+3*i, drgb+3*i+1, drgb+3*i+2, 
                             tyiq, tyiq+1,tyiq+2);
    for (j = 0; j < 3; j++) {
      if (type_yiq == NCL_double) {
        dyiq[3*i+j] = tyiq[j];
      }
      else {
        fyiq[3*i+j] = (float)tyiq[j];
      }
    } 
  } 

/*
 * free memory.
 */
  if((void*)drgb != rgb) NclFree(drgb);
  NclFree(tyiq);

/*
 * Return.
 */
  if (type_yiq == NCL_double) {
    return(NclReturnValue(dyiq,ndims_rgb, dsizes_rgb, NULL, type_yiq, 0));
  }
  else {
    return(NclReturnValue(fyiq,ndims_rgb, dsizes_rgb, NULL, type_yiq, 0));
  }
}

NhlErrorTypes yiqrgb_W( void )
{
/*
 * Input variables
 */
  void *yiq;
  double *dyiq;
  ng_size_t dsizes_yiq[NCL_MAX_DIMENSIONS];
  int ndims_yiq;
  NclBasicDataTypes type_yiq;
 
/*
 * output variable 
 */
  float *frgb = NULL;
  double *drgb = NULL;
  ng_size_t size_rgb;
  NclBasicDataTypes type_rgb;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i,j;
  double *trgb;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  yiq = (void*)NclGetArgValue(
          0,
          1,
          &ndims_yiq,
          dsizes_yiq,
          NULL,
          NULL,
          &type_yiq,
          DONT_CARE);


/*
 * Check dimension sizes. The argument must be an array with
 * rightmost dimension 3.
 */

/*
 * Check rightmost dimension size for yiq.
 */
  if(dsizes_yiq[ndims_yiq-1] != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"yiqrgb: the last (rightmost) dimension of the input array must be of size 3");
    return(NhlFATAL);
  }

/*
 * Determine size for the return array.
 */
  size_rgb = 1;
  for (i = 0; i < ndims_yiq-1; i++) {
    size_rgb *= dsizes_yiq[i];
  }

/*
 * Coerce input variables to double if necessary.
 */
  dyiq  = coerce_input_double(yiq, type_yiq, 3*size_rgb, 0, NULL, NULL);
  if(dyiq == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"yiqrgb: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 *  Allocate space for output array.
 */
  type_rgb = NCL_float;
  if (type_yiq == NCL_double) {
    type_rgb = NCL_double;
    drgb = (double *)calloc(3*size_rgb, sizeof(double));
    if(drgb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"yiqrgb: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }
  else {
    frgb = (float *)calloc(3*size_rgb, sizeof(float));
    if(frgb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"yiqrgb: Unable to allocate memory for return.");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  trgb = (double *) calloc(3,sizeof(double));
  for( i = 0; i < size_rgb; i++ ) {
    NGCALLF(dyiqrgb,DYIQRGB)(dyiq+3*i, dyiq+3*i+1, dyiq+3*i+2, 
                             trgb, trgb+1,trgb+2);
    for (j = 0; j < 3; j++) {
      if (type_rgb == NCL_double) {
        drgb[3*i+j] = trgb[j];
      }
      else {
        frgb[3*i+j] = (float)trgb[j];
      }
    } 
  } 

/*
 * free memory.
 */
  if((void*)dyiq != yiq) NclFree(dyiq);
  NclFree(trgb);

/*
 * Return.
 */
  if (type_rgb == NCL_double) {
    return(NclReturnValue(drgb,ndims_yiq, dsizes_yiq, NULL, type_rgb, 0));
  }
  else {
    return(NclReturnValue(frgb,ndims_yiq, dsizes_yiq, NULL, type_rgb, 0));
  }
}
