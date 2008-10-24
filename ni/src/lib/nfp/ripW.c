#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcapecalc3d,DCAPECALC3D)(double *prs, double *tmk, 
                                             double *qvp, double *ght,
                                             double *ter, double *sfp, 
                                             double *cape, double *cin, 
                                             int *miy, int *mjx, int *mkzh, 
                                             int *i3dflag, int *ter_follow,
                                             char *,int);


extern void NGCALLF(calcdbz,CALCDBZ)(double *, double *, double *,
                                     double *, double *, double *,
                                     double *, double *, int *,
                                     int *, int *, int *);

/*
 * Function for calculating cape (from the RIP code). This function
 * depends on the "psadilookup.dat" file, which by default will be
 * searched for in $NCARG_ROOT/lib/ncarg/data/asc/), unless
 * NCARG_PSADILOOKUP is set to the location of this file.
 */

/*
 * The rip_cape_3d wrapper is for the case where I3DFLAG is set to
 * 1 in the Fortran rip_cape.f file.
 */
NhlErrorTypes rip_cape_3d_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *q, *z, *zsfc, *psfc;
  logical *ter_follow;
  double *tmp_p, *tmp_t, *tmp_q, *tmp_z, *tmp_zsfc, *tmp_psfc;
  int ndims_p, ndims_t, ndims_q, ndims_z, ndims_zsfc, ndims_psfc;
  int dsizes_p[NCL_MAX_DIMENSIONS], dsizes_t[NCL_MAX_DIMENSIONS];
  int dsizes_q[NCL_MAX_DIMENSIONS], dsizes_z[NCL_MAX_DIMENSIONS];
  int dsizes_zsfc[NCL_MAX_DIMENSIONS], dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_t, type_q, type_z, type_zsfc, type_psfc;

/*
 * Output array variables
 */
  void *cape;
  double *tmp_cape, *tmp_cin;
  NclBasicDataTypes type_cape;
  int ndims_cape, *dsizes_cape;
/*
 * File input variables.
 */
  const char *path = NULL;
  char psa_file[_NhlMAXFNAMELEN];

/*
 * Declare various variables for random purposes.
 */
  int i, miy, mjx, mkzh, ntime, size_cape, size_output, size_zsfc;
  int i3dflag=1, scalar_zsfc, index_cape, index_zsfc, index_cin;
  int iter, ret;

/*
 * The default is to use $NCARG_ROOT/lib/ncarg/data/asc/psadilookup.dat
 * for the input data file, unless PSADILOOKUP_PATH is set by the
 * user, then it will try to use this path. 
 */
  path = getenv("PSADILOOKUP_PATH");
  if ((void *)path == (void *)NULL) {
    path = _NGGetNCARGEnv("data");
    if ((void *)path != (void *)NULL) {
      strcpy(psa_file,path);
      strcat(psa_file,_NhlPATHDELIMITER);
      strcat(psa_file,"asc");
      strcat(psa_file,_NhlPATHDELIMITER);
      strcat(psa_file,"psadilookup.dat");
    }
  }
  else {
    strcpy(psa_file,path);
    strcat(psa_file,_NhlPATHDELIMITER);
    strcat(psa_file,"psadilookup.dat");
  }

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 */
  p = (void*)NclGetArgValue(
          0,
          7,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);

  t = (void*)NclGetArgValue(
          1,
          7,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);


  q = (void*)NclGetArgValue(
          2,
          7,
          &ndims_q,
          dsizes_q,
          NULL,
          NULL,
          &type_q,
          2);

  z = (void*)NclGetArgValue(
          3,
          7,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          &type_z,
          2);

  zsfc = (void*)NclGetArgValue(
          4,
          7,
          &ndims_zsfc,
          dsizes_zsfc,
          NULL,
          NULL,
          &type_zsfc,
          2);

  psfc = (void*)NclGetArgValue(
          5,
          7,
          &ndims_psfc,
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);

  ter_follow = (logical*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
  
  if(*ter_follow) iter = 1;
  else            iter = 0;

/*
 * Check the input dimension sizes. There are three possible cases
 * for the input dimension sizes:
 *
 *  - p,t,q,z (time,lev,lat,lon) and psfc,zsfc (time,lat,lon)
 *  - p,t,q,z (lev,lat,lon) and psfc,zsfc (lat,lon)
 *  - p,t,q,z (lev) and psfc,zsfc (scalars)
 */
  if(ndims_p != ndims_t || ndims_p != ndims_q || ndims_p != ndims_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: The p, t, q, and z arrays must all have the same number of dimensions");
    return(NhlFATAL);
  }
  if(ndims_p != 1 && ndims_p != 3 && ndims_p != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: The p, t, q, and z arrays must be 1-, 3-, or 4-dimensional\n");
    return(NhlFATAL);
  }
/*
 * zsfc and psfc can be scalars, if the other input arrays are 1D.
 */
  scalar_zsfc = is_scalar(ndims_zsfc,dsizes_zsfc);

  if((ndims_zsfc != ndims_psfc) || (scalar_zsfc && ndims_p != 1) || 
     (!scalar_zsfc && ndims_zsfc != ndims_p-1)) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: The zsfc and psfc arrays must have the same number of dimensions, and either be scalars or one less dimension than the other input arrays");
    return(NhlFATAL);
  }

/*
 * Now check that the dimension sizes are equal to each other.
 */
  for(i = 0; i < ndims_p; i++) {
    if(dsizes_p[i] != dsizes_t[i] || dsizes_p[i] != dsizes_q[i] || 
       dsizes_p[i] != dsizes_z[i]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: p, t, q, and z must be the same dimensionality");
    return(NhlFATAL);
    }
  }

  for(i = 0; i < ndims_psfc; i++) {
    if(dsizes_psfc[i] != dsizes_zsfc[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: psfc and zsfc must be the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Get sizes of input arrays.
 */
  if(ndims_p == 4) {
    ntime = dsizes_p[0];          /* time, serves as a leftmost dimension */
    mkzh  = dsizes_p[1];          /* lev */
    mjx   = dsizes_p[2];          /* lat */
    miy   = dsizes_p[3];          /* lon */
  }
  else if(ndims_p == 3) {
    ntime = 1;
    mkzh = dsizes_p[0];           /* lev */
    mjx  = dsizes_p[1];           /* lat */
    miy  = dsizes_p[2];           /* lon */
  }
  else if(ndims_p == 1) {
    ntime = 1;
    mkzh = dsizes_p[0];           /* lev */
    mjx  = 1;                     /* lat */
    miy  = 1;                     /* lon */
  }

/*
 * Check some more dimension sizes.
 */
  if(ndims_p == 4) {
    if(dsizes_psfc[0] != ntime || dsizes_psfc[1] != mjx || 
       dsizes_psfc[2] != miy) { 
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: If p,q,t,z are 4-dimensional (time x lev x lat x lon), psfc,zsfc must be 3-dimensional (time x lat x lon)");
      return(NhlFATAL);
    }
  }
  if(ndims_p == 3) {
    if(dsizes_psfc[0] != mjx || dsizes_psfc[1] != miy) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: If p,q,t,z are 3-dimensional (time x lev x lat x lon), psfc,zsfc must be 2-dimensional (lat x lon)");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of output array. The output array size depends on
 * the size of p,t,q,z:
 *
 *  - p,t,q,z (time,lev,lat,lon) and psfc,zsfc (time,lat,lon)
 *       output array: (2,time,lev,lat,lon)
 *  - p,t,q,z (lev,lat,lon) and psfc,zsfc (lat,lon)
 *       output array: (2,lev,lat,lon)
 *  - p,t,q,z (lev) and psfc,zsfc (scalars)
 *       output array: (2,lev)
 */
  ndims_cape = ndims_p+1;
  dsizes_cape = (int *)calloc(ndims_cape,sizeof(int));
  if(dsizes_cape == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for array dimensionality");
    return(NhlFATAL);
  }

  dsizes_cape[0] = 2;                /* 0 = cape, 1 = cin */
  for(i = 0; i < ndims_p; i++ ) {
    dsizes_cape[i+1] = dsizes_p[i];
  }
  size_zsfc   = mjx * miy;
  size_cape   = mkzh * size_zsfc;       /* Also size of cin array */
  size_output = 2 * size_cape * ntime;

/* 
 * Allocate space for output arrays.  If any of the input is already double,
 * then we don't need to allocate space for temporary arrays, because
 * we'll just change the pointer into the void array appropriately.
 */
  if(type_p == NCL_double || type_t == NCL_double || type_q == NCL_double ||
     type_z == NCL_double) {
    type_cape = NCL_double;
    cape = (double *)calloc(size_output,sizeof(double));
    if(cape == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_cape = NCL_float;
    cape      = (float *)calloc(size_output,sizeof(float));
    tmp_cape  = (double *)calloc(size_cape,sizeof(double));
    tmp_cin   = (double *)calloc(size_cape,sizeof(double));
    if(cape == NULL || tmp_cape == NULL || tmp_cin == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for allocating input arrays to double, if necessary.
 */
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(size_cape,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(size_cape,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_q != NCL_double) {
    tmp_q = (double *)calloc(size_cape,sizeof(double));
    if(tmp_q == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_z != NCL_double) {
    tmp_z = (double *)calloc(size_cape,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_zsfc != NCL_double) {
    tmp_zsfc = (double *)calloc(size_zsfc,sizeof(double));
    if(tmp_zsfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_psfc != NCL_double) {
    tmp_psfc = (double *)calloc(size_zsfc,sizeof(double));
    if(tmp_psfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_3d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran routine.
 */ 
  index_cape = index_zsfc = 0;
  index_cin = ntime * size_cape;    /* Second half of output array */

  for(i = 0; i < ntime; i++) {
/*
 * Coerce subset of input arrays to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_cape,type_p,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[index_cape];
    }
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_cape,type_t,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[index_cape];
    }
    if(type_q != NCL_double) {
      coerce_subset_input_double(q,tmp_q,index_cape,type_q,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_q to appropriate location in q.
 */
      tmp_q = &((double*)q)[index_cape];
    }
    if(type_z != NCL_double) {
      coerce_subset_input_double(z,tmp_z,index_cape,type_z,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_cape];
    }

    if(type_psfc != NCL_double) {
      coerce_subset_input_double(psfc,tmp_psfc,index_zsfc,type_psfc,
                                 size_zsfc,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[index_zsfc];
    }
    if(type_zsfc != NCL_double) {
      coerce_subset_input_double(zsfc,tmp_zsfc,index_zsfc,type_zsfc,
                                 size_zsfc,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zsfc to appropriate location in zsfc.
 */
      tmp_zsfc = &((double*)zsfc)[index_zsfc];
    }
    
/*
 * Point tmp_cape and tmp_cin to appropriate location in cape
 * if necessary
 */
    if(type_cape == NCL_double) {
      tmp_cape = &((double*)cape)[index_cape];
      tmp_cin  = &((double*)cape)[index_cin];
    }
    
/*
 * Call Fortran routine.
 */
    NGCALLF(dcapecalc3d,DCAPECALC3D)(tmp_p, tmp_t, tmp_q, tmp_z, tmp_zsfc,
                                     tmp_psfc, tmp_cape, tmp_cin, &miy,
                                     &mjx, &mkzh, &i3dflag, &iter,
                                     psa_file,strlen(psa_file));
/*
 * If the output is to be float, then do the coercion here.
 */
    if(type_cape == NCL_float) {
      coerce_output_float_only(cape,tmp_cape,size_cape,index_cape);
      coerce_output_float_only(cape,tmp_cin,size_cape,index_cin);
    }
/*
 * Implement the pointers into the arrays.
 */
    index_cape += size_cape;
    index_cin  += size_cape;
    index_zsfc += size_zsfc;
  }
/*
 * Free memory.
 */
  if(type_p != NCL_double) NclFree(tmp_p);
  if(type_t != NCL_double) NclFree(tmp_t);
  if(type_q != NCL_double) NclFree(tmp_q);
  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zsfc != NCL_double) NclFree(tmp_zsfc);
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_cape != NCL_double) NclFree(tmp_cape);
  if(type_cape != NCL_double) NclFree(tmp_cin);
/*
 * Set up variable to return.
 */
  ret = NclReturnValue(cape,ndims_cape,dsizes_cape,NULL,type_cape,0);
  NclFree(dsizes_cape);
  return(ret);
}


/*
 * The rip_cape_2d wrapper is for the case where I3DFLAG is set to
 * 0 in the Fortran rip_cape.f file.  In this case, 4 2D arrays
 * are returned: cape, cin, lcl, and lfc, but they are all returned 
 * in one big array whose leftmost dimension is 4:
 *
 *   index 0 = cape
 *   index 1 = cin
 *   index 2 = lcl
 *   index 3 = lfc
 */
NhlErrorTypes rip_cape_2d_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *q, *z, *zsfc, *psfc;
  logical *ter_follow;
  double *tmp_p, *tmp_t, *tmp_q, *tmp_z, *tmp_zsfc, *tmp_psfc;
  int ndims_p, ndims_t, ndims_q, ndims_z, ndims_zsfc, ndims_psfc;
  int dsizes_p[NCL_MAX_DIMENSIONS], dsizes_t[NCL_MAX_DIMENSIONS];
  int dsizes_q[NCL_MAX_DIMENSIONS], dsizes_z[NCL_MAX_DIMENSIONS];
  int dsizes_zsfc[NCL_MAX_DIMENSIONS], dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_t, type_q, type_z, type_zsfc, type_psfc;

/*
 * Output array variables
 */
  void *cape;
  double *tmp_cape, *tmp_cin;
  NclBasicDataTypes type_cape;
  int ndims_cape, *dsizes_cape;
/*
 * File input variables.
 */
  const char *path = NULL;
  char psa_file[_NhlMAXFNAMELEN];

/*
 * Declare various variables for random purposes.
 */
  int i, miy, mjx, mkzh, ntime, size_cape, size_output, size_zsfc;
  int size_left_zsfc, i3dflag=0, scalar_zsfc, index_cape, index_zsfc;
  int index_output_cape, index_output_cin, index_output_lcl;
  int index_output_lfc, mkzh0_index, mkzh1_index, mkzh2_index;
  int iter, ret;

/*
 * The default is to use $NCARG_ROOT/lib/ncarg/data/asc/psadilookup.dat
 * for the input data file, unless PSADILOOKUP_PATH is set by the
 * user, then it will try to use this path. 
 */
  path = getenv("PSADILOOKUP_PATH");
  if ((void *)path == (void *)NULL) {
    path = _NGGetNCARGEnv("data");
    if ((void *)path != (void *)NULL) {
      strcpy(psa_file,path);
      strcat(psa_file,_NhlPATHDELIMITER);
      strcat(psa_file,"asc");
      strcat(psa_file,_NhlPATHDELIMITER);
      strcat(psa_file,"psadilookup.dat");
    }
  }
  else {
    strcpy(psa_file,path);
    strcat(psa_file,_NhlPATHDELIMITER);
    strcat(psa_file,"psadilookup.dat");
  }

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 */
  p = (void*)NclGetArgValue(
          0,
          7,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);

  t = (void*)NclGetArgValue(
          1,
          7,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);


  q = (void*)NclGetArgValue(
          2,
          7,
          &ndims_q,
          dsizes_q,
          NULL,
          NULL,
          &type_q,
          2);

  z = (void*)NclGetArgValue(
          3,
          7,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          &type_z,
          2);

  zsfc = (void*)NclGetArgValue(
          4,
          7,
          &ndims_zsfc,
          dsizes_zsfc,
          NULL,
          NULL,
          &type_zsfc,
          2);

  psfc = (void*)NclGetArgValue(
          5,
          7,
          &ndims_psfc,
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);

  ter_follow = (logical*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
  
  if(*ter_follow) iter = 1;
  else            iter = 0;


/*
 * Check the input dimension sizes. There are two possible cases
 * for the input dimension sizes:
 *
 *  - p,t,q,z (time,lev,lat,lon) and psfc,zsfc (time,lat,lon)
 *  - p,t,q,z (lev,lat,lon) and psfc,zsfc (lat,lon)
 */
  if(ndims_p != ndims_t || ndims_p != ndims_q || ndims_p != ndims_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: The p, t, q, and z arrays must all have the same number of dimensions");
    return(NhlFATAL);
  }
  if(ndims_p != 3 && ndims_p != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: The p, t, q, and z arrays must be 3 or 4-dimensional\n");
    return(NhlFATAL);
  }
/*
 * Check zsfc and psfc dimension sizes.
 */
  if((ndims_zsfc != ndims_psfc) || (ndims_zsfc != ndims_p-1)) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: The zsfc and psfc arrays must have the same number of dimensions and be one less dimension than the other input arrays");
    return(NhlFATAL);
  }

/*
 * Now check that the dimension sizes are equal to each other.
 */
  for(i = 0; i < ndims_p; i++) {
    if(dsizes_p[i] != dsizes_t[i] || dsizes_p[i] != dsizes_q[i] || 
       dsizes_p[i] != dsizes_z[i]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: p, t, q, and z must be the same dimensionality");
    return(NhlFATAL);
    }
  }

  for(i = 0; i < ndims_psfc; i++) {
    if(dsizes_psfc[i] != dsizes_zsfc[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: psfc and zsfc must be the same dimensionality");
      return(NhlFATAL);
    }
  }
  if(ndims_p == 4) {
/*
 * Store dimension sizes.
 */
    ntime = dsizes_p[0];       /* time */
    mkzh = dsizes_p[1];        /* lev */
    mjx  = dsizes_p[2];        /* lat */
    miy  = dsizes_p[3];        /* lon */
    ndims_cape = 4;
    if(dsizes_psfc[0] != ntime || dsizes_psfc[1] != mjx ||
       dsizes_psfc[2] != miy) { 
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: If p,q,t,z are 4-dimensional (time x lev x lat x lon), psfc,zsfc must be 3-dimensional (time x lat x lon)");
      return(NhlFATAL);

    }
  }
  else if(ndims_p == 3) {
/*
 * Store dimension sizes.
 */
    ntime = 1;
    mkzh = dsizes_p[0];           /* lev */
    mjx  = dsizes_p[1];           /* lat */
    miy  = dsizes_p[2];           /* lon */
    ndims_cape = 3;
    if(dsizes_psfc[0] != mjx || dsizes_psfc[1] != miy) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: If p,q,t,z are 3-dimensional (time x lev x lat x lon), psfc,zsfc must be 2-dimensional (lat x lon)");
      return(NhlFATAL);
    }
  }
/*
 * If mkzh is not at least size 3, then this dimension won't be big 
 * enough to contain the cin, lcl, and lfc values.
 */
  if(mkzh < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: The level dimension must have at least 3 elements");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array. The output array size depends on
 * the size of p,t,q,z:
 *
 *  - p,t,q,z (time,lev,lat,lon) and psfc,zsfc (time,lat,lon)
 *       output array: (4,time,lat,lon)
 *  - p,t,q,z (lev,lat,lon) and psfc,zsfc (lat,lon)
 *       output array: (4,lat,lon)
 */
  dsizes_cape = (int *)calloc(ndims_cape,sizeof(int));
  if(dsizes_cape == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for array dimensionality");
    return(NhlFATAL);
  }

  dsizes_cape[0]            = 4;    /* To hold the 4 different variables. */
                                    /* 0=cape, 1=cin, 2=lcl, 3=lfc */
  dsizes_cape[ndims_cape-1] = miy;
  dsizes_cape[ndims_cape-2] = mjx;
  if(ndims_cape == 4) dsizes_cape[1] = ntime;

  size_zsfc   = mjx * miy;
  size_cape   = mkzh * size_zsfc;
  mkzh0_index = (mkzh-1) * size_zsfc;    /* Indexes into cin array for   */
  mkzh1_index = (mkzh-2) * size_zsfc;    /* returning cin, lcl, and lfc  */
  mkzh2_index = (mkzh-3) * size_zsfc;    /* respectively. */
  size_left_zsfc = size_zsfc * ntime;
  size_output = 4 * size_left_zsfc;

/* 
 * Allocate space for output and temporary arrays.  Even if the input
 * arrays are already double, go ahead and allocate some space for
 * them b/c we have to copy the values back to 4 different locations.
 */
  if(type_p == NCL_double || type_t == NCL_double || type_q == NCL_double ||
     type_z == NCL_double) {
    type_cape = NCL_double;
    cape      = (double *)calloc(size_output,sizeof(double));
  }
  else {
    type_cape = NCL_float;
    cape      = (float *)calloc(size_output,sizeof(float));
  }
  tmp_cape = (double *)calloc(size_cape,sizeof(double));
  tmp_cin  = (double *)calloc(size_cape,sizeof(double));
  if(cape == NULL || tmp_cape == NULL || tmp_cin == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

/*
 * Allocate memory for allocating input arrays to double, if necessary.
 */
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(size_cape,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(size_cape,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_q != NCL_double) {
    tmp_q = (double *)calloc(size_cape,sizeof(double));
    if(tmp_q == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_z != NCL_double) {
    tmp_z = (double *)calloc(size_cape,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_zsfc != NCL_double) {
    tmp_zsfc = (double *)calloc(size_zsfc,sizeof(double));
    if(tmp_zsfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_psfc != NCL_double) {
    tmp_psfc = (double *)calloc(size_zsfc,sizeof(double));
    if(tmp_psfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape_2d: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran routine.
 */ 
  index_cape        = index_zsfc = 0;
  index_output_cape = 0;
  index_output_cin  = size_left_zsfc;
  index_output_lcl  = 2 * size_left_zsfc;
  index_output_lfc  = 3 * size_left_zsfc;

  for(i = 0; i < ntime; i++) {
/*
 * Coerce subset of input arrays to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_cape,type_p,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[index_cape];
    }
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_cape,type_t,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[index_cape];
    }
    if(type_q != NCL_double) {
      coerce_subset_input_double(q,tmp_q,index_cape,type_q,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_q to appropriate location in q.
 */
      tmp_q = &((double*)q)[index_cape];
    }
    if(type_z != NCL_double) {
      coerce_subset_input_double(z,tmp_z,index_cape,type_z,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_cape];
    }

    if(type_psfc != NCL_double) {
      coerce_subset_input_double(psfc,tmp_psfc,index_zsfc,type_psfc,
                                 size_zsfc,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[index_zsfc];
    }
    if(type_zsfc != NCL_double) {
      coerce_subset_input_double(zsfc,tmp_zsfc,index_zsfc,type_zsfc,
                                 size_zsfc,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zsfc to appropriate location in zsfc.
 */
      tmp_zsfc = &((double*)zsfc)[index_zsfc];
    }
    
/*
 * Call Fortran routine.
 */
    NGCALLF(dcapecalc3d,DCAPECALC3D)(tmp_p, tmp_t, tmp_q, tmp_z, tmp_zsfc,
                                     tmp_psfc, tmp_cape, tmp_cin, &miy,
                                     &mjx, &mkzh, &i3dflag, &iter,
                                     psa_file,strlen(psa_file));
/*
 * Copy the values back out to the correct places in the "cape" array.
 *
 * This is a bit whacky, because the Fortran code is doing something
 * fancy to save memory. The "tmp_cin" array contains the cin values in
 * the last mkzh section, the lcl values in the 2nd-to-last mkzh
 * section, and the lfc values in the 3rd-to-last mkzh section.
 *
 * The "tmp_cape" array contains its values in the last mkzh section
 * of the tmp_cape array.
 */
    coerce_output_float_or_double(cape,&tmp_cape[mkzh0_index],type_cape,
                                  size_zsfc,index_output_cape);
    coerce_output_float_or_double(cape,&tmp_cin[mkzh0_index],type_cape,
                                  size_zsfc,index_output_cin);
    coerce_output_float_or_double(cape,&tmp_cin[mkzh1_index],type_cape,
                                  size_zsfc,index_output_lcl);
    coerce_output_float_or_double(cape,&tmp_cin[mkzh2_index],type_cape,
                                  size_zsfc,index_output_lfc);
/*
 * Implement the pointers into the arrays.
 */
    index_cape += size_cape;
    index_zsfc += size_zsfc;
    index_output_cape += size_zsfc;
    index_output_cin  += size_zsfc;
    index_output_lcl  += size_zsfc;
    index_output_lfc  += size_zsfc;
  }
/*
 * Free memory.
 */
  if(type_p != NCL_double) NclFree(tmp_p);
  if(type_t != NCL_double) NclFree(tmp_t);
  if(type_q != NCL_double) NclFree(tmp_q);
  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zsfc != NCL_double) NclFree(tmp_zsfc);
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  NclFree(tmp_cape);
  NclFree(tmp_cin);
/*
 * Set up variable to return.
 */
  ret = NclReturnValue(cape,ndims_cape,dsizes_cape,NULL,type_cape,0);
  NclFree(dsizes_cape);
  return(ret);
}

NhlErrorTypes rip_reflectivity_W( void )
{

/*
 * Input variables
 */
/*
 * There are 8 arguments (0-7).
 *
 * Argument # 0
 */
  void *dbz;
  double *tmp_dbz;
  int ndims_dbz, dsizes_dbz[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_dbz;

/*
 * Argument # 1
 */
  void *prs;
  double *tmp_prs;
  int ndims_prs, dsizes_prs[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_prs;

/*
 * Argument # 2
 */
  void *tmk;
  double *tmp_tmk;
  int ndims_tmk, dsizes_tmk[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_tmk;

/*
 * Argument # 3
 */
  void *qvp;
  double *tmp_qvp;
  int ndims_qvp, dsizes_qvp[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_qvp;

/*
 * Argument # 4
 */
  void *qra;
  double *tmp_qra;
  int ndims_qra, dsizes_qra[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_qra;

/*
 * Argument # 5
 */
  void *qsn;
  double *tmp_qsn;
  int ndims_qsn, dsizes_qsn[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_qsn;

/*
 * Argument # 6
 */
  void *qgr;
  double *tmp_qgr;
  int ndims_qgr, dsizes_qgr[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_qgr;

/*
 * Argument # 7
 */
  int *opt;
/*
 * Return variable
 */
  void *mdbz;
  double *tmp_mdbz;
  int ndims_mdbz, *dsizes_mdbz;
  NclBasicDataTypes type_mdbz;

/*
 * Various
 */
  int west_east_dim, south_north_dim, bottom_top_dim, n_we_sn_bt, n_we_sn;
  int index_dbz, index_mdbz, ivarint = 0;
  int i, ndims_leftmost, size_leftmost, size_output, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  dbz = (void*)NclGetArgValue(
           0,
           8,
           &ndims_dbz,
           dsizes_dbz,
           NULL,
           NULL,
           &type_dbz,
           2);

/*
 * Check dimension sizes.
 */
  if(ndims_dbz != 3 && ndims_dbz != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: The dbz array must be 3 or 4 dimensions");
    return(NhlFATAL);
  }

  bottom_top_dim  = dsizes_dbz[ndims_dbz-3];
  south_north_dim = dsizes_dbz[ndims_dbz-2];
  west_east_dim   = dsizes_dbz[ndims_dbz-1];
  n_we_sn         = west_east_dim * south_north_dim;
  n_we_sn_bt      = n_we_sn * bottom_top_dim;
  if(ndims_dbz == 4) {
    size_leftmost  = dsizes_dbz[0];
    ndims_leftmost = 1;
  }
  else {
    size_leftmost  = 1;
    ndims_leftmost = 0;
  }
    
/*
 * Get argument # 1
 */
  prs = (void*)NclGetArgValue(
           1,
           8,
           &ndims_prs,
           dsizes_prs,
           NULL,
           NULL,
           &type_prs,
           2);

/*
 * Get argument # 2
 */
  tmk = (void*)NclGetArgValue(
           2,
           8,
           &ndims_tmk,
           dsizes_tmk,
           NULL,
           NULL,
           &type_tmk,
           2);

/*
 * Get argument # 3
 */
  qvp = (void*)NclGetArgValue(
           3,
           8,
           &ndims_qvp,
           dsizes_qvp,
           NULL,
           NULL,
           &type_qvp,
           2);

/*
 * Get argument # 4
 */
  qra = (void*)NclGetArgValue(
           4,
           8,
           &ndims_qra,
           dsizes_qra,
           NULL,
           NULL,
           &type_qra,
           2);

/*
 * Get argument # 5
 */
  qsn = (void*)NclGetArgValue(
           5,
           8,
           &ndims_qsn,
           dsizes_qsn,
           NULL,
           NULL,
           &type_qsn,
           2);

/*
 * Get argument # 6
 */
  qgr = (void*)NclGetArgValue(
           6,
           8,
           &ndims_qgr,
           dsizes_qgr,
           NULL,
           NULL,
           &type_qgr,
           2);

/*
 * Check dimension sizes of the rest of the input arrays.
 */
  if(ndims_prs != ndims_dbz || ndims_tmk != ndims_dbz ||
     ndims_qvp != ndims_dbz || ndims_qra != ndims_dbz ||
     ndims_qsn != ndims_dbz || ndims_qgr != ndims_dbz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: The input arrays must have the same number of dimensions as dbz");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_dbz; i++) {
    if(dsizes_prs[i] != dsizes_dbz[i] || dsizes_tmk[i] != dsizes_dbz[i] ||
       dsizes_qvp[i] != dsizes_dbz[i] || dsizes_qra[i] != dsizes_dbz[i] ||
       dsizes_qsn[i] != dsizes_dbz[i] || dsizes_qgr[i] != dsizes_dbz[i]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: The input arrays must have the same dimensionality as dbz");
    }
  }

/*
 * Get argument # 7
 */
  opt = (int*)NclGetArgValue(
           7,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * The output type defaults to float, unless any of the input arrays
 * are double.
 */
  type_mdbz = NCL_float;

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_dbz.
 */
  if(type_dbz != NCL_double) {
    tmp_dbz = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_dbz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing dbz array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }
/*
 * Allocate space for tmp_prs.
 */
  if(type_prs != NCL_double) {
    tmp_prs = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_prs == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing prs array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }
/*
 * Allocate space for tmp_tmk.
 */
  if(type_tmk != NCL_double) {
    tmp_tmk = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_tmk == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing tmk array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }
/*
 * Allocate space for tmp_qvp.
 */
  if(type_qvp != NCL_double) {
    tmp_qvp = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_qvp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing qvp array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }
/*
 * Allocate space for tmp_qra.
 */
  if(type_qra != NCL_double) {
    tmp_qra = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_qra == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing qra array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }
/*
 * Allocate space for tmp_qsn.
 */
  if(type_qsn != NCL_double) {
    tmp_qsn = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_qsn == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing qsn array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }
/*
 * Allocate space for tmp_qgr.
 */
  if(type_qgr != NCL_double) {
    tmp_qgr = (double *)calloc(n_we_sn_bt,sizeof(double));
    if(tmp_qgr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for coercing qgr array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mdbz = NCL_double;
  }

/*
 * Calculate size of output array.
 */
  size_output = size_leftmost * n_we_sn;

/* 
 * Allocate space for output array.
 */
  if(type_mdbz != NCL_double) {
    mdbz = (void *)calloc(size_output, sizeof(float));
    tmp_mdbz = (double *)calloc(n_we_sn,sizeof(double));
    if(tmp_mdbz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    mdbz = (void *)calloc(size_output, sizeof(double));
  }
  if(mdbz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_mdbz = ndims_leftmost + 2;
  dsizes_mdbz = (int*)calloc(ndims_mdbz,sizeof(int));  
  if( dsizes_mdbz == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_reflectivity: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_mdbz[ndims_mdbz-2]         = south_north_dim;
  dsizes_mdbz[ndims_mdbz-1]         = west_east_dim;
  if(ndims_dbz == 4) dsizes_mdbz[0] = dsizes_dbz[0];

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_dbz = index_mdbz = 0;

  for(i = 0; i < size_leftmost; i++) {

/*
 * Coerce subsection of dbz (tmp_dbz) to double if necessary.
 */
    if(type_dbz != NCL_double) {
      coerce_subset_input_double(dbz,tmp_dbz,index_dbz,type_dbz,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_dbz = &((double*)dbz)[index_dbz];
    }

/*
 * Coerce subsection of prs (tmp_prs) to double if necessary.
 */
    if(type_prs != NCL_double) {
      coerce_subset_input_double(prs,tmp_prs,index_dbz,type_prs,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_prs = &((double*)prs)[index_dbz];
    }

/*
 * Coerce subsection of tmk (tmp_tmk) to double if necessary.
 */
    if(type_tmk != NCL_double) {
      coerce_subset_input_double(tmk,tmp_tmk,index_dbz,type_tmk,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_tmk = &((double*)tmk)[index_dbz];
    }

/*
 * Coerce subsection of qvp (tmp_qvp) to double if necessary.
 */
    if(type_qvp != NCL_double) {
      coerce_subset_input_double(qvp,tmp_qvp,index_dbz,type_qvp,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_qvp = &((double*)qvp)[index_dbz];
    }

/*
 * Coerce subsection of qra (tmp_qra) to double if necessary.
 */
    if(type_qra != NCL_double) {
      coerce_subset_input_double(qra,tmp_qra,index_dbz,type_qra,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_qra = &((double*)qra)[index_dbz];
    }

/*
 * Coerce subsection of qsn (tmp_qsn) to double if necessary.
 */
    if(type_qsn != NCL_double) {
      coerce_subset_input_double(qsn,tmp_qsn,index_dbz,type_qsn,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_qsn = &((double*)qsn)[index_dbz];
    }

/*
 * Coerce subsection of qgr (tmp_qgr) to double if necessary.
 */
    if(type_qgr != NCL_double) {
      coerce_subset_input_double(qgr,tmp_qgr,index_dbz,type_qgr,n_we_sn_bt,
                                 0,NULL,NULL);
    }
    else {
      tmp_qgr = &((double*)qgr)[index_dbz];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_mdbz == NCL_double) tmp_mdbz = &((double*)mdbz)[index_mdbz];

/*
 * Call the Fortran routine.
 */
    NGCALLF(calcdbz,CALCDBZ)(tmp_mdbz, tmp_dbz, tmp_prs, tmp_tmk,
                             tmp_qvp, tmp_qra, tmp_qsn, tmp_qgr,
                             &west_east_dim, &south_north_dim,
                             &bottom_top_dim, &ivarint);

/*
 * Coerce output back to float if necessary.
 */
    if(type_mdbz == NCL_float) {
      coerce_output_float_only(mdbz,tmp_mdbz,n_we_sn,index_mdbz);
    }
    index_dbz  += n_we_sn_bt;
    index_mdbz += n_we_sn;
  }

/*
 * Free unneeded memory.
 */
  if(type_dbz != NCL_double) NclFree(tmp_dbz);
  if(type_prs != NCL_double) NclFree(tmp_prs);
  if(type_tmk != NCL_double) NclFree(tmp_tmk);
  if(type_qvp != NCL_double) NclFree(tmp_qvp);
  if(type_qra != NCL_double) NclFree(tmp_qra);
  if(type_qsn != NCL_double) NclFree(tmp_qsn);
  if(type_qgr != NCL_double) NclFree(tmp_qgr);
  if(type_mdbz != NCL_double) NclFree(tmp_mdbz);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(mdbz,ndims_mdbz,dsizes_mdbz,NULL,type_mdbz,0);

  NclFree(dsizes_mdbz);
  return(ret);
}
