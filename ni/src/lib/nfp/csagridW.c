#include <stdio.h>
#include <string.h>
/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>

/*
 *  Declare externals.
 */
extern float *c_csa1s(int, float [], float [], int, int, float [], int *);
extern float *c_csa1xs(int, float [], float [], float [], int,
                    float, int, int, float xo[], int *);
extern float *c_csa2s(int, float [], float [], float [], int [],
                      int, int, float [], float [], int *);
extern float *c_csa2xs(int, float [], float [], float [], float [], int [], 
                       float, int [], int, int, float [], float [], int *);
extern float *c_csa2ls(int, float [], float [], float [], int [],
                       int, float [], float [], int *);
extern float *c_csa2lxs(int, float [], float [], float [], float [], int [], 
                        float, int [], int, float [], float [], int *);
extern float *c_csa3s(int, float [], float [], float [], float [], int [], 
                      int, int, int, float [], float [], float [], int *);
extern float *c_csa3xs(int, float [], float [], float [], float [], float [], 
                       int [], float, int [], int, int, int, float [], 
                       float [], float [], int *);
extern float *c_csa3ls(int, float [], float [], float [], float [],
                       int [], int, float [], float [], float[], int *);
extern float *c_csa3lxs(int, float [], float [], float [], float [],
                        float [], int [], float, int [],
                        int, float [], float [], float [], int *);

char csamsg[61];

NhlErrorTypes csa1xs_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *smth;
  int ndims_smth, dsizes_smth[NCL_MAX_DIMENSIONS];
  int *nderiv;
  int ndims_nderiv, dsizes_nderiv[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];

  float *yo;

  NclBasicDataTypes type_xi, type_yi, type_wts, type_knots, 
                    type_smth, type_nderiv, type_xo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 7, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 7, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (weights).
 */
  wts = (float *) NclGetArgValue(2, 7, &ndims_wts, dsizes_wts, NULL, NULL, 
                                &type_wts, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_wts != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_wts != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 7, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (smoothing option).
 */
  smth = (float *) NclGetArgValue(4, 7, &ndims_smth, dsizes_smth, NULL, NULL, 
                                &type_smth, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_smth != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_smth != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(5, 7, &ndims_nderiv, dsizes_nderiv, NULL, 
                                   NULL, &type_nderiv, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_nderiv != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_nderiv != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(6, 7, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float));
  yo = c_csa1xs(dsizes_xi[0], xi, yi, wts, *knots, *smth, *nderiv,
           dsizes_xo[0], xo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa1xs: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( (void *) yo, 1, dsizes_xo, NULL, NCL_float, 0));
}

NhlErrorTypes csa1s_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];

  float *yo;

  NclBasicDataTypes type_xi, type_yi, type_knots, type_xo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 4, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1s: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1s: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 4, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1xs: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (knots).
 */
  knots = (int *) NclGetArgValue(2, 4, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1s: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1s: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(3, 4, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1s: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa1s: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float));
  yo = c_csa1s(dsizes_xi[0], xi, yi, *knots, dsizes_xo[0], xo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa1s: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( (void *) yo, 1, dsizes_xo, NULL, NCL_float, 0));
}

NhlErrorTypes csa2s_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo;
  int ndims_zo, dsizes_zo[2];

  float *output;

  NclBasicDataTypes type_xi, type_yi, type_zi, type_knots, type_xo, type_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 6, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2s: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 6, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2s: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2s: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2s: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(4, 6, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2s: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(5, 6, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2s: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  output = c_csa2s(dsizes_xi[0], xi, yi, zi, knots,
                    dsizes_xo[0], dsizes_yo[0], xo, yo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa2s: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_zo = 2;
  dsizes_zo[0] = dsizes_xo[0];
  dsizes_zo[1] = dsizes_yo[0];
  return(NclReturnValue( (void *) output,  ndims_zo, dsizes_zo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa2xs_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *smth;
  int ndims_smth, dsizes_smth[NCL_MAX_DIMENSIONS];
  int *nderiv;
  int ndims_nderiv, dsizes_nderiv[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo;
  int ndims_zo, dsizes_zo[2];

  float *output;

  NclBasicDataTypes type_xi, type_yi, type_zi, type_wts, type_knots, 
                    type_smth, type_nderiv, type_xo, type_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 9, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 9, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (weights).
 */
  wts = (float *) NclGetArgValue(3, 9, &ndims_wts, dsizes_wts, NULL, NULL, 
                                &type_wts, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_wts != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_wts != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (float *) NclGetArgValue(5, 9, &ndims_smth, dsizes_smth, NULL, NULL, 
                                &type_smth, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_smth != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_smth != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, &ndims_nderiv, dsizes_nderiv, NULL, 
                                   NULL, &type_nderiv, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_nderiv != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_nderiv != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(7, 9, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #7 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(8, 9, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #8.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Argument #8 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #8.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2xs: Argument #8 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  output = c_csa2xs(dsizes_xi[0], xi, yi, zi, wts, knots, *smth, nderiv,
                    dsizes_xo[0], dsizes_yo[0], xo, yo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa2xs: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_zo = 2;
  dsizes_zo[0] = dsizes_xo[0];
  dsizes_zo[1] = dsizes_yo[0];
  return(NclReturnValue( (void *) output,  ndims_zo, dsizes_zo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa2ls_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo;
  int ndims_zo, dsizes_zo[1];

  float *output;

  NclBasicDataTypes type_xi, type_yi, type_zi, type_knots, type_xo, type_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 6, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2ls: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 6, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2ls: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2ls: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2ls: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(4, 6, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2ls: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(5, 6, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2ls: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  output = c_csa2ls(dsizes_xi[0], xi, yi, zi, knots,
                    dsizes_xo[0], xo, yo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa2ls: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_zo = 1;
  dsizes_zo[0] = dsizes_xo[0];
  return(NclReturnValue( (void *) output,  ndims_zo, dsizes_zo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa2lxs_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *smth;
  int ndims_smth, dsizes_smth[NCL_MAX_DIMENSIONS];
  int *nderiv;
  int ndims_nderiv, dsizes_nderiv[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo;
  int ndims_zo, dsizes_zo[1];

  float *output;

  NclBasicDataTypes type_xi, type_yi, type_zi, type_wts, type_knots, 
                    type_smth, type_nderiv, type_xo, type_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 9, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 9, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (weights).
 */
  wts = (float *) NclGetArgValue(3, 9, &ndims_wts, dsizes_wts, NULL, NULL, 
                                &type_wts, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_wts != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_wts != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (float *) NclGetArgValue(5, 9, &ndims_smth, dsizes_smth, NULL, NULL, 
                                &type_smth, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_smth != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_smth != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, &ndims_nderiv, dsizes_nderiv, NULL, 
                                   NULL, &type_nderiv, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_nderiv != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_nderiv != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(7, 9, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #7 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(8, 9, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #8.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Argument #8 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #8.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa2lxs: Argument #8 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  output = c_csa2lxs(dsizes_xi[0], xi, yi, zi, wts, knots, *smth, nderiv,
                    dsizes_xo[0], xo, yo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa2lxs: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_zo = 1;
  dsizes_zo[0] = dsizes_xo[0];
  return(NclReturnValue( (void *) output,  ndims_zo, dsizes_zo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa3xs_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *smth;
  int ndims_smth, dsizes_smth[NCL_MAX_DIMENSIONS];
  int *nderiv;
  int ndims_nderiv, dsizes_nderiv[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int ndims_zo, dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo;
  int ndims_uo, dsizes_uo[3];

  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui, type_wts, type_knots, 
                    type_smth, type_nderiv, type_xo, type_yo, type_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 11, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 11, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 11, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                &type_ui, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ui != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_ui != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (weights).
 */
  wts = (float *) NclGetArgValue(4, 11, &ndims_wts, dsizes_wts, NULL, NULL, 
                                &type_wts, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_wts != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_wts != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (float *) NclGetArgValue(6, 11, &ndims_smth, dsizes_smth, NULL, NULL, 
                                &type_smth, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_smth != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_smth != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, &ndims_nderiv, dsizes_nderiv, NULL, 
                                   NULL, &type_nderiv, 2);
 
/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_nderiv != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
  if (type_nderiv != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #7 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(8, 11, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #8.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #8 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #8.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #8 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(9, 11, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #9.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #9 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #9.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #9 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(10, 11, &ndims_zo, dsizes_zo, NULL, NULL, 
                                &type_zo, 2);
 
/*
 * Check number of dimensions for argument #10.
 */
  if(ndims_zo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Argument #10 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #10.
 */
  if (type_zo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3xs: Argument #10 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  uo = c_csa3xs(dsizes_xi[0], xi, yi, zi, ui, wts, knots, *smth, nderiv,
                    dsizes_xo[0], dsizes_yo[0], dsizes_zo[0], xo, yo, zo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa3xs: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_uo = 3;
  dsizes_uo[0] = dsizes_xo[0];
  dsizes_uo[1] = dsizes_yo[0];
  dsizes_uo[2] = dsizes_zo[0];
  return(NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa3s_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int ndims_zo, dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo;
  int ndims_uo, dsizes_uo[3];

  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui, type_knots, 
                    type_xo, type_yo, type_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 8, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 8, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 8, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                &type_ui, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ui != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_ui != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(5, 8, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(6, 8, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(7, 8, &ndims_zo, dsizes_zo, NULL, NULL, 
                                &type_zo, 2);
 
/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_zo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
  if (type_zo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3s: Argument #7 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  uo = c_csa3s(dsizes_xi[0], xi, yi, zi, ui, knots,
                   dsizes_xo[0], dsizes_yo[0], dsizes_zo[0], xo, yo, zo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa3s: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_uo = 3;
  dsizes_uo[0] = dsizes_xo[0];
  dsizes_uo[1] = dsizes_yo[0];
  dsizes_uo[2] = dsizes_zo[0];
  return(NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa3lxs_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *smth;
  int ndims_smth, dsizes_smth[NCL_MAX_DIMENSIONS];
  int *nderiv;
  int ndims_nderiv, dsizes_nderiv[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int ndims_zo, dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo;
  int ndims_uo, dsizes_uo[1];

  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui, type_wts, type_knots, 
                    type_smth, type_nderiv, type_xo, type_yo, type_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 11, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 11, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 11, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                &type_ui, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ui != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_ui != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (weights).
 */
  wts = (float *) NclGetArgValue(4, 11, &ndims_wts, dsizes_wts, NULL, NULL, 
                                &type_wts, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_wts != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_wts != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (float *) NclGetArgValue(6, 11, &ndims_smth, dsizes_smth, NULL, NULL, 
                                &type_smth, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_smth != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_smth != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, &ndims_nderiv, dsizes_nderiv, NULL, 
                                   NULL, &type_nderiv, 2);
 
/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_nderiv != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
  if (type_nderiv != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #7 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(8, 11, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #8.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #8 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #8.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #8 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(9, 11, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #9.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #9 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #9.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #9 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(10, 11, &ndims_zo, dsizes_zo, NULL, NULL, 
                                &type_zo, 2);
 
/*
 * Check number of dimensions for argument #10.
 */
  if(ndims_zo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Argument #10 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #10.
 */
  if (type_zo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3lxs: Argument #10 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  uo = c_csa3lxs(dsizes_xi[0], xi, yi, zi, ui, wts, knots, *smth, nderiv,
                 dsizes_xo[0], xo, yo, zo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa3lxs: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_uo = 1;
  dsizes_uo[0] = dsizes_xo[0];
  return(NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
         NULL, NCL_float, 0));
}

NhlErrorTypes csa3ls_W(void)
{
  int ier = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  int ndims_knots, dsizes_knots[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int ndims_zo, dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo;
  int ndims_uo, dsizes_uo[1];

  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui, type_knots, 
                    type_xo, type_yo, type_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 8, &ndims_xi, dsizes_xi, NULL, NULL, 
                                &type_xi, 2);
 
/*
 * Check number of dimensions for argument #0.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #0 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #0.
 */
  if (type_xi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #0 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 8, &ndims_yi, dsizes_yi, NULL, NULL, 
                                &type_yi, 2);
 
/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
  if (type_yi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #1 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 8, &ndims_zi, dsizes_zi, NULL, NULL, 
                                &type_zi, 2);
 
/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_zi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                &type_ui, 2);
 
/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ui != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_ui != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #2 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, &ndims_knots, dsizes_knots, NULL, NULL,
                                   &type_knots, 2);
 
/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_knots != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
  if (type_knots != NCL_int) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #4 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(5, 8, &ndims_xo, dsizes_xo, NULL, NULL, 
                                &type_xo, 2);
 
/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
  if (type_xo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #5 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(6, 8, &ndims_yo, dsizes_yo, NULL, NULL, 
                                &type_yo, 2);
 
/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
  if (type_yo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #6 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(7, 8, &ndims_zo, dsizes_zo, NULL, NULL, 
                                &type_zo, 2);
 
/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_zo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
  if (type_zo != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
            "csa3ls: Argument #7 has an incorrect type.");
    return(NhlFATAL);
  }

/*
 *  Call the C procedure.
 */
  uo = c_csa3ls(dsizes_xi[0], xi, yi, zi, ui, knots,
                dsizes_xo[0], xo, yo, zo, &ier);
  if (ier != 0) {
    sprintf(csamsg, "c_csa3ls: Error number %d.", ier);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
    return(NhlFATAL);
  }
  ndims_uo = 1;
  dsizes_uo[0] = dsizes_xo[0];
  return(NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, 
         NULL, NCL_float, 0));
}
