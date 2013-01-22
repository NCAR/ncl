#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>              /* isdigit() */
#include "convert_datum.h"
#include "wrapper.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502884197169399375105820974944592308
#endif

#ifndef M_PI_2
#define M_PI_2  1.57079632679489661923
#endif


NhlErrorTypes utm2latlon_W( void )
{
/*
 * Input array variables
 */
  void *xy;
  double *tmp_xy;
  int *datum;
  int ndims_xy;
  ng_size_t dsizes_xy[NCL_MAX_DIMENSIONS];
  int has_missing_xy;
  NclScalar missing_xy, missing_dxy;
  NclBasicDataTypes type_xy;

/*
 * Variables for retrieving attributes from "xy"
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  NrmQuark *grid_zone = NULL;
  ng_size_t total_size_grid_zone = 0;
/*
 * Output variables.
 */
  void *latlon;
  NclScalar missing_latlon;
  NclBasicDataTypes type_latlon;
/*
 * various
 */
  ng_size_t i, total_size_xy, npts, inpts;
  int ret, ier;
  int zlen, bad_datum = 0, found_grid_zone = 0;
  UTM utm;
  LL ll;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  xy = (void*)NclGetArgValue(
           0,
           2,
           &ndims_xy, 
           dsizes_xy,
           &missing_xy,
           &has_missing_xy,
           &type_xy,
           DONT_CARE);

  if(dsizes_xy[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"utm2latlon: The leftmost dimension of 'xy' must be 2");
    return(NhlFATAL);
  }

/*
 * Get datum.
 */
  datum = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);

  if(*datum < 0 || *datum > 2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon: Unrecognized datum. All missing values will be returned.");
    bad_datum = 1;
  }

/*
 * Calculate size of input array.
 */
  npts = 1;
  for( i = 1; i < ndims_xy; i++ ) npts *= dsizes_xy[i];
  total_size_xy = 2 * npts;     /* We know first dimension is 2. */

/*
 * Calculate size and dimensions for output array, and allocate
 * memory for output array.
 */
  if(type_xy != NCL_double) {
    type_latlon    = NCL_float;
    missing_latlon = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    latlon         = (float *)calloc(total_size_xy,sizeof(float));
  }
  else {
    type_latlon    = NCL_double;
    missing_latlon = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
    latlon         = (double *)calloc(total_size_xy,sizeof(double));
  }

/*
 * Make sure we have enough memory for output.
 */
  if( latlon == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"utm2latlon: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Coerce missing values to double.
 */
  coerce_missing(type_xy,has_missing_xy,&missing_xy,&missing_dxy,NULL);

/* 
 * The "grid_zone" attribute of "xy" must be set, otherwise missing
 * values will be returned.
 */
  stack_entry = _NclGetArg(0, 2, DONT_CARE);
  switch (stack_entry.kind) {
  case NclStk_VAR:
    if (stack_entry.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
      if (attr_obj == NULL) {
        break;
      }
    }
    else {
/*
 * att_id == -1 ==> no optional args given.
 */
      break;
    }
/* 
 * Get optional arguments. If none are specified, then return
 * missing values.
 */
    if (attr_obj->att.n_atts == 0) {
      break;
    }
    else {
/*
 * Get list of attributes.
 */
      attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them.
 */
      while (attr_list != NULL) {
        if ((strcmp(attr_list->attname, "grid_zone")) == 0) {
/*
 * If we find grid_zone, we have to make sure it is a scalar, or 
 * the same size as xy.
 */
          grid_zone = (NrmQuark *) attr_list->attvalue->multidval.val;
          total_size_grid_zone = 1;
          for( i = 0; i < attr_list->attvalue->multidval.n_dims; i++ ) 
            total_size_grid_zone *= attr_list->attvalue->multidval.dim_sizes[i];
          if(total_size_grid_zone == 1 || total_size_grid_zone == npts) {
            found_grid_zone = 1;
          }
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }
  if(!found_grid_zone) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon: The 'grid_zone' attribute was either not set or not the right size. All missing values will be returned.");
  }
/* 
 * If we reach this point and either the datum was unrecognized, or
 * "grid_zone" was either invalid or wasn't set, then we have to
 * return all missing values.
 */
  if(!found_grid_zone || bad_datum) { 
    if(type_latlon == NCL_float) {
      for(i = 0; i < total_size_xy; i++ ) {
        ((float*)latlon)[i] = missing_latlon.floatval;
      }
    }
    else {
      for(i = 0; i < total_size_xy; i++ ) {
        ((double*)latlon)[i] = missing_latlon.doubleval;
      }
    }
/*
 * Return all missing values.
 */
    ret = NclReturnValue(latlon,ndims_xy,dsizes_xy,&missing_latlon,type_latlon,0);
    return(ret);
  }
            
/*
 * Convert input to double if necessary.
 */
  tmp_xy = coerce_input_double(xy,type_xy,total_size_xy,has_missing_xy,
                  &missing_xy,&missing_dxy);

/* 
 * Loop through each element.
 */
  for( i = 0; i < npts; i++ ) {
    inpts = i + npts;
    ier   = 0;
    if(  total_size_grid_zone > 1 ||
         (total_size_grid_zone == 1 && i==0)) {
      zlen = strlen(NrmQuarkToString(grid_zone[i]));
      if(0 < zlen && zlen < GRID_ZONE_LENGTH) {
        
        strcpy(utm.grid_zone,NrmQuarkToString(grid_zone[i]));
      }
      else {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon: The grid_zone '%s' is invalid. Missing values will be returned for this subset.",NrmQuarkToString(grid_zone[i]));
        ier = -1;
      }
    }
    if(!ier && (!has_missing_xy ||
       (has_missing_xy && (tmp_xy[i]     != missing_dxy.doubleval &&
                           tmp_xy[inpts] != missing_dxy.doubleval)))) {

      utm.x = tmp_xy[i];
      utm.y = tmp_xy[inpts];
      ier   = utm2ll (&utm, &ll, (unsigned char)(*datum));

      if(!ier) {
        coerce_output_float_or_double(latlon,&ll.latitude, type_latlon,1,i);
        coerce_output_float_or_double(latlon,&ll.longitude,type_latlon,1,inpts);
      }
    }
    else {
      ier = -1;
    }
    if(ier) {
      if(type_latlon == NCL_float) {
        ((float*)latlon)[i]     = missing_latlon.floatval;
        ((float*)latlon)[inpts] = missing_latlon.floatval;
      }
      else {
        ((double*)latlon)[i]     = missing_latlon.doubleval;
        ((double*)latlon)[inpts] = missing_latlon.doubleval;
      }
    }
  }

/*
 * Free the work arrays.
 */

  if(type_xy != NCL_double) NclFree(tmp_xy);

/*
 * Return.
 */ 
  if(has_missing_xy) {
    ret = NclReturnValue(latlon,ndims_xy,dsizes_xy,&missing_latlon,type_latlon,0);
  }
  else {
    ret = NclReturnValue(latlon,ndims_xy,dsizes_xy,NULL,type_latlon,0);
  }
  return(ret);
}


NhlErrorTypes latlon2utm_W( void )
{
/*
 * Input array variables
 */
  void *latlon;
  double *tmp_latlon;
  int *datum;
  int ndims_latlon;
  ng_size_t dsizes_latlon[NCL_MAX_DIMENSIONS];
  int has_missing_latlon;
  NclScalar missing_latlon, missing_dlatlon;
  NclBasicDataTypes type_latlon;

/*
 * Output variables.
 */
  void *xy;
  NclScalar missing_xy;
  NclBasicDataTypes type_xy;
  NclObjClass type_obj_xy;
/*
 * various
 */
  ng_size_t i, total_size_latlon, npts, inpts;
  int ret, ier;
  UTM utm;
  LL ll;

/*
 * Attribute variable for returning "grid_zone"
 */
  NclQuark *grid_zone;
  int att_id;
  ng_size_t dsizes_grid_zone[NCL_MAX_DIMENSIONS], ndims_grid_zone;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  latlon = (void*)NclGetArgValue(
           0,
           2,
           &ndims_latlon, 
           dsizes_latlon,
           &missing_latlon,
           &has_missing_latlon,
           &type_latlon,
           DONT_CARE);

  if(dsizes_latlon[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"latlon2utm: The leftmost dimension of 'latlon' must be 2");
    return(NhlFATAL);
  }

/*
 * Get datum.
 */
  datum = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);

/*
 * Calculate size of input array.
 */
  npts = 1;
  for( i = 1; i < ndims_latlon; i++ ) {
    npts *= dsizes_latlon[i];
    dsizes_grid_zone[i-1] = dsizes_latlon[i];
  }
  if(ndims_latlon > 1) {
    ndims_grid_zone     = ndims_latlon-1;
  }
  else {
    dsizes_grid_zone[0] = 1;
    ndims_grid_zone     = 1;
  }
  total_size_latlon = 2 * npts;     /* We know first dimension is 2. */

/*
 * Calculate size and dimensions for output array, and allocate
 * memory for output array.
 */
  if(type_latlon != NCL_double) {
    type_xy     = NCL_float;
    type_obj_xy = nclTypefloatClass;
    missing_xy  = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    xy          = (float *)calloc(total_size_latlon,sizeof(float));
  }
  else {
    type_xy     = NCL_double;
    type_obj_xy = nclTypedoubleClass;
    missing_xy  = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
    xy          = (double *)calloc(total_size_latlon,sizeof(double));
  }
  grid_zone = (NclQuark *)calloc(npts,sizeof(NclQuark));

/*
 * Make sure we have enough memory for output.
 */
  if( xy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"latlon2utm: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Coerce missing values to double.
 */
  coerce_missing(type_latlon,has_missing_latlon,&missing_latlon,&missing_dlatlon,NULL);

/*
 * Return all missing if not a recognized datum.
 */
  if(*datum < 0 || *datum > 2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"latlon2utm: Unrecognized datum. All missing values will be returned.");

    if(type_xy == NCL_float) {
      for(i = 0; i < total_size_latlon; i++ ) {
        ((float*)xy)[i] = missing_xy.floatval;
      }
    }
    else {
      for(i = 0; i < total_size_latlon; i++ ) {
        ((double*)xy)[i] = missing_xy.doubleval;
      }
    }
/*
 * Return all missing values.
 */
    ret = NclReturnValue(xy,ndims_latlon,dsizes_latlon,&missing_xy,type_xy,0);
    return(ret);
  }

/*
 * Convert input to double if necessary.
 */
  tmp_latlon = coerce_input_double(latlon,type_latlon,total_size_latlon,
                                   has_missing_latlon,&missing_latlon,
                                   &missing_dlatlon);

/* 
 * Loop through each element.
 */
  for( i = 0; i < npts; i++ ) {
    inpts = i + npts;
    if(!has_missing_latlon ||
       (has_missing_latlon && 
        (tmp_latlon[i]     != missing_dlatlon.doubleval &&
         tmp_latlon[inpts] != missing_dlatlon.doubleval))) {

      ll.latitude  = tmp_latlon[i];
      ll.longitude = tmp_latlon[inpts];
      ier          = ll2utm (&ll, &utm, (unsigned char)(*datum));

      if(!ier) {
        coerce_output_float_or_double(xy,&utm.x,type_xy,1,i);
        coerce_output_float_or_double(xy,&utm.y,type_xy,1,inpts);
        grid_zone[i] = NrmStringToQuark(utm.grid_zone);
      }
    }
    else {
      ier = -1;
    }
    if(ier) {
      grid_zone[i] = NrmStringToQuark("missing");
      if(type_xy == NCL_float) {
        ((float*)xy)[i]     = missing_xy.floatval;
        ((float*)xy)[inpts] = missing_xy.floatval;
      }
      else {
        ((double*)xy)[i]     = missing_xy.doubleval;
        ((double*)xy)[inpts] = missing_xy.doubleval;
      }
    }
  }

/*
 * Free the work arrays.
 */
  if(type_latlon != NCL_double) NclFree(tmp_latlon);

/*
 * Set up variable to return.
 */
  if(has_missing_latlon) {
        return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            xy,
                            &missing_xy,
                            ndims_latlon,
                            dsizes_latlon,
                            TEMPORARY,
                            NULL,
                            type_obj_xy
                            );
  }
  else {
        return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            xy,
                            NULL,
                            ndims_latlon,
                            dsizes_latlon,
                            TEMPORARY,
                            NULL,
                            type_obj_xy
                            );
  }
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)grid_zone,
                         NULL,
                         ndims_grid_zone,
                         dsizes_grid_zone,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "grid_zone",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);

}

/*
 * Peter Daly
 * MIT Ocean Acoustics
 * pmd@mit.edu
 * 25-MAY-1998
 * 
 * These routines convert UTM to Lat/Longitude and vice-versa,
 * using the WGS-84 (GPS standard) or Clarke 1866 Datums.
 * 
 * The formulae for these routines were originally taken from
 * Chapter 10 of "GPS: Theory and Practice," by B. Hofmann-Wellenhof,
 * H. Lictenegger, and J. Collins. (3rd ed) ISBN: 3-211-82591-6,
 * however, several errors were present in the text which
 * made their formulae incorrect.
 *
 * Instead, the formulae for these routines was taken from
 * "Map Projections: A Working Manual," by John P. Snyder
 * (US Geological Survey Professional Paper 1395)
 *
 * Copyright (C) 1998 Massachusetts Institute of Technology
 *               All Rights Reserved
 *
 * RCS ID: $Id: convert_datum.c,v 1.6 2009-04-23 21:37:06 haley Exp $
 */

/*
int
main (int argc, char **argv)
{
  UTM utm;
  LL ll;

  ll.latitude = 34.0 + 6/60.0 + 41.03/3600.0;
  ll.longitude = -(119 + 19/60.0 + 49.90/3600.0);

  utm.x = utm.y = 0.0;

  ll2utm (&ll, &utm, WGS_84_DATUM);

  printf ("ll: %f %f utm: [%s] %f,%f\n",
          ll.latitude, ll.longitude,
          utm.grid_zone, utm.x, utm.y);

  ll2utm (&utm, &ll, WGS_84_DATUM);

  printf ("ll: %f %f utm: [%s] %f,%f\n",
          ll.latitude, ll.longitude,
          utm.grid_zone, utm.x, utm.y);

  return(0);
}
*/

void
get_grid_zone (LL *ll, char grid_zone[GRID_ZONE_LENGTH], double *lambda0)
{
  unsigned int zone_long;
  unsigned char zone_lat;

  /* Solve for the grid zone, returns the central meridian */

  /* First, let's take care of the polar regions */

  if (ll->latitude < -80) {
    if (ll->longitude < 0) {
      sprintf (grid_zone, "30A");
      *lambda0 = 0 * M_PI / 180.0;
    } else {
      sprintf (grid_zone, "31B");
      *lambda0 = 0 * M_PI / 180.0;
    }
    return;

  } else if (ll->latitude > 84) {
    if (ll->longitude < 0) {
      sprintf (grid_zone, "30Y");
      *lambda0 = 0 * M_PI / 180.0;
    } else {
      sprintf (grid_zone, "31Z");
      *lambda0 = 0 * M_PI / 180.0;
    }
    return;
  }

  /* Now the special "X" grid */

  if (ll->latitude > 72 && ll->longitude > 0 && ll->longitude < 42) {
    if (ll->longitude < 9) {
      *lambda0 = 4.5;
      sprintf (grid_zone, "31X");
    } else if (ll->longitude < 21) {
      *lambda0 = 15 * M_PI / 180.0;
      sprintf (grid_zone, "33X");
    } else if (ll->longitude < 33) {
      *lambda0 = 27 * M_PI / 180.0;
      sprintf (grid_zone, "35X");
    } else if (ll->longitude < 42) {
      *lambda0 = 37.5 * M_PI / 180.0;
      sprintf (grid_zone, "37X");
    }
  
    return;
  }

  /* Handle the special "V" grid */

  if (ll->latitude > 56 && ll->latitude < 64 &&
      ll->longitude > 0 && ll->longitude < 12) {
    if (ll->longitude < 3) {
      *lambda0 = 1.5 * M_PI / 180.0;
      sprintf (grid_zone, "31V");
    } else if (ll->longitude < 12) {
      *lambda0 = 7.5 * M_PI / 180.0;
      sprintf (grid_zone, "32V");
    }
      
    return;
  }

  /* The remainder of the grids follow the standard rule */

  zone_long = (unsigned int) ((ll->longitude - (-180.0)) / 6.0) + 1;
  *lambda0 = ((zone_long - 1) * 6.0 + (-180.0) + 3.0) * M_PI / 180.0;

  zone_lat = (unsigned char) ((ll->latitude - (-80.0)) / 8.0) + 'C';
  if (zone_lat > 'H')
    zone_lat++;
  if (zone_lat > 'N')
    zone_lat++;

  if (ll->latitude > 80)
    zone_lat = 'X';

  grid_zone[0] = ((unsigned char) (zone_long / 10)) + '0';
  grid_zone[1] = (zone_long % 10) + '0';
  grid_zone[2] = zone_lat;
  grid_zone[3] = 0;

  /* All done */
}

int
get_lambda0 (char grid_zone[GRID_ZONE_LENGTH], double *lambda0)
{
  unsigned int zone_long;
  unsigned char zone_lat;

  /* given the grid zone, sets the central meridian, lambda0 */

  /* Check the grid zone format */

  if (!isdigit(grid_zone[0]) || !isdigit(grid_zone[1])) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon/latlon2utm: Invalid grid zone format: [%s]\n",
             grid_zone);
    return(-1);
  }

  zone_long = (grid_zone[0] - '0') * 10 + (grid_zone[1] - '0');
  zone_lat = grid_zone[2];

  /* Take care of special cases */

  switch(zone_lat) {
  case 'A': case 'B': case 'Y': case 'Z':
    *lambda0 = 0;
    return(0);
    break;
  case 'V':
    switch (zone_long) {
    case 31:
      *lambda0 = 1.5 * M_PI / 180.0;
      return(0);
      break;
    case 32:
      *lambda0 = 7.5 * M_PI / 180.0;
      return(0);
      break;
    break;
    }
  case 'X':
    switch (zone_long) {
    case 31:
      *lambda0 = 4.5 * M_PI / 180.0;
      return(0);
      break;
    case 33:
      *lambda0 = 15 * M_PI / 180.0;
      return(0);
      break;
    case 35:
      *lambda0 = 27 * M_PI / 180.0;
      return(0);
      break;
    case 37:
      *lambda0 = 37.5 * M_PI / 180.0;
      return(0);
      break;
    case 32: case 34: case 36:
      NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon/latlon2utm: Zone %02d%c does not exist!\n",
               zone_long,
               zone_lat);
      return(-1);
      break;
    }
    break;
  }

  /* Now handle standard cases */

  *lambda0 = ((zone_long - 1) * 6.0 + (-180.0) + 3.0) * M_PI / 180.0;

  /* All done */

  return(0);
}

int
ll2utm(LL *ll, UTM *utm, unsigned char datum)
{
  double a, b, f, e, e2, e4, e6;
  double phi, lambda, lambda0, phi0, k0;
  double t, rho, m, x, y, k, mm, mm0;
  double aa, aa2, aa3, aa4, aa5, aa6;
  double ep2, nn, tt, cc;
  char grid_zone[GRID_ZONE_LENGTH];

  /* Converts lat/long to UTM, using the specified datum */

  switch (datum) {
  case CLARKE_1866_DATUM:
    a = 6378206.4;
    b = 6356583.8;
    break;
  case GRS_80_DATUM:
    a = 6378137;
    b = 6356752.3;
    break;
  case WGS_84_DATUM:
    a = 6378137.0;              /* semimajor axis of ellipsoid (meters) */
    b = 6356752.31425;          /* semiminor axis of ellipsoid (meters) */
    break;
  default:
    NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon/latlon2utm: Unknown datum: %d\n",
            datum);
   return(-1);
  }

  /* Calculate flatness and eccentricity */

  f = 1 - (b/a);
  e2 = 2*f - f*f;
  e = sqrt(e2);
  e4 = e2*e2;
  e6 = e4*e2;

  /* Convert latitude/longitude to radians */
  
  phi = (ll->latitude) * M_PI / 180.0;
  lambda = (ll->longitude) * M_PI / 180.0;

  /* Figure out the UTM zone, as well as lambda0 */

  (void)get_grid_zone (ll, grid_zone, &lambda0);
  phi0 = 0.0;

  /* See if this will use UTM or UPS */

  if (ll->latitude > 84.0) {

    /* use Universal Polar Stereographic Projection (north polar aspect) */

    k0 = 0.994;

    t = sqrt(((1-sin(phi))/(1+sin(phi))) *
             pow((1+e*sin(phi))/(1-e*sin(phi)), e));
    rho = 2 * a * k0 * t / sqrt(pow(1+e,1+e) * pow(1-e,1-e));
    m = cos(phi) / sqrt (1 - e2 * sin(phi)*sin(phi));

    x = rho * sin(lambda - lambda0);
    y = -rho * cos(lambda - lambda0);
    k = rho * a * m;

    /* Apply false easting/northing */

    x += 2000000;
    y += 2000000;

  } else if (ll->latitude < -80.0) {

    /* use Universal Polar Stereographic Projection (south polar aspect) */

    phi = -phi;
    lambda = -lambda;
    lambda0 = -lambda0;

    k0 = 0.994;

    t = sqrt(((1-sin(phi))/(1+sin(phi))) *
             pow((1+e*sin(phi))/(1-e*sin(phi)), e));
    rho = 2 * a * k0 * t / sqrt(pow(1+e,1+e) * pow(1-e,1-e));
    m = cos(phi) / sqrt (1 - e2 * sin(phi)*sin(phi));

    x = rho * sin(lambda - lambda0);
    y = -rho * cos(lambda - lambda0);
    k = rho * a * m;

    x = -x;
    y = -y;

    /* Apply false easting/northing */

    x += 2000000;
    y += 2000000;

  } else {

    /* Use UTM */

    /* set scale on central median (0.9996 for UTM) */
    
    k0 = 0.9996;

    mm = a * ((1-e2/4 - 3*e4/64 - 5*e6/256) * phi -
              (3*e2/8 + 3*e4/32 + 45*e6/1024) * sin(2*phi) +
              (15*e4/256 + 45*e6/1024) * sin(4*phi) -
              (35*e6/3072) * sin(6*phi));

    mm0 = a * ((1-e2/4 - 3*e4/64 - 5*e6/256) * phi0 -
               (3*e2/8 + 3*e4/32 + 45*e6/1024) * sin(2*phi0) +
               (15*e4/256 + 45*e6/1024) * sin(4*phi0) -
               (35*e6/3072) * sin(6*phi0));

    aa = (lambda - lambda0) * cos(phi);
    aa2 = aa*aa;
    aa3 = aa2*aa;
    aa4 = aa2*aa2;
    aa5 = aa4*aa;
    aa6 = aa3*aa3;

    ep2 = e2 / (1 - e2);
    nn = a / sqrt(1 - e2*sin(phi)*sin(phi));
    tt = tan(phi) * tan(phi);
    cc = ep2 * cos(phi) * cos(phi);

    k = k0 * (1 + (1+cc)*aa2/2 + (5-4*tt+42*cc+13*cc*cc-28*ep2)*aa4/24.0
              + (61-148*tt+16*tt*tt)*aa6/720.0);
    x = k0 * nn * (aa + (1-tt+cc)*aa3/6 +
                   (5-18*tt+tt*tt+72*cc-58*ep2)*aa5/120.0);
    y = k0 * (mm - mm0 + nn * tan(phi) * 
              (aa2/2 + (5-tt+9*cc+4*cc*cc)*aa4/24.0
               + (61 - 58*tt + tt*tt + 600*cc - 330*ep2)*aa6/720));

    /* Apply false easting and northing */

    x += 500000.0;
    if (y < 0.0)
      y += 10000000;
  }

  /* Set entries in UTM structure */

  memcpy (utm->grid_zone, grid_zone, GRID_ZONE_LENGTH);
  utm->x = x;
  utm->y = y;

  /* done */

  return(0);
}

int
utm2ll (UTM *utm, LL *ll, unsigned char datum)
{
  double a, b, f, e, e2, e4, e6, e8;
  double lambda0, x, y, k0, rho, t, chi, phi, phi1, phit;
  double lambda, phi0, e1, e12, e13, e14;
  double mm, mm0, mu, ep2, cc1, tt1, nn1, rr1;
  double dd, dd2, dd3, dd4, dd5, dd6;

  unsigned int zone_long;
  unsigned char zone_lat;

  /* Converts UTM to lat/long, using the specified datum */

  switch (datum) {
  case CLARKE_1866_DATUM:
    a = 6378206.4;
    b = 6356583.8;
    break;
  case GRS_80_DATUM:
    a = 6378137;
    b = 6356752.3;
    break;
  case WGS_84_DATUM:
    a = 6378137.0;              /* semimajor axis of ellipsoid (meters) */
    b = 6356752.31425;          /* semiminor axis of ellipsoid (meters) */
    break;
  default:
    NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon/latlon2utm: Unknown datum: %d\n",
            datum);
   return(-1);
  }

  /* Calculate flatness and eccentricity */

  f = 1 - (b/a);
  e2 = 2*f - f*f;
  e = sqrt(e2);
  e4 = e2*e2;
  e6 = e4*e2;
  e8 = e4*e4;

  /* Given the UTM grid zone, generate a baseline lambda0 */

  if (get_lambda0 (utm->grid_zone, &lambda0) < 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"utm2latlon/latlon2utm: unable to translate UTM to LL\n");
    return(-1);
  }

  zone_long = (utm->grid_zone[0] - '0') * 10 + (utm->grid_zone[1] - '0');
  zone_lat = utm->grid_zone[2];

  /* Take care of the polar regions first. */

  switch(zone_lat) {
  case 'Y': case 'Z':           /* north polar aspect */

    /* Subtract the false easting/northing */

    x = utm->x - 2000000;
    y = utm->y - 2000000;

    /* Solve for inverse equations */

    k0 = 0.994;
    rho = sqrt (x*x + y*y);
    t = rho * sqrt(pow(1+e,1+e) * pow(1-e,1-e)) / (2*a*k0);

    /* Solve for latitude and longitude */

    chi = M_PI_2 - 2 * atan(t);
    phit = chi + (e2/2 + 5*e4/24 + e6/12 + 13*e8/360) * sin(2*chi) +
      (7*e4/48 + 29*e6/240 + 811*e8/11520) * sin(4*chi) +
      (7*e6/120 + 81*e8/1120) * sin(6*chi) +
      (4279*e8/161280) * sin(8*chi);

    do {
      phi = phit;
      phit = M_PI_2 - 2*atan(t*pow((1-e*sin(phi))/(1+e*sin(phi)),e/2));
    } while (fabs(phi-phit) > LOWER_EPS_LIMIT);

    lambda = lambda0 + atan2(x,-y);
    break;

  case 'A': case 'B':           /* south polar aspect */

    /* Subtract the false easting/northing */

    x = -(utm->x - 2000000);
    y = -(utm->y - 2000000);

    /* Solve for inverse equations */

    k0 = 0.994;
    rho = sqrt (x*x + y*y);
    t = rho * sqrt(pow(1+e,1+e) * pow(1-e,1-e)) / (2*a*k0);

    /* Solve for latitude and longitude */

    chi = M_PI_2 - 2 * atan(t);
    phit = chi + (e2/2 + 5*e4/24 + e6/12 + 13*e8/360) * sin(2*chi) +
      (7*e4/48 + 29*e6/240 + 811*e8/11520) * sin(4*chi) +
      (7*e6/120 + 81*e8/1120) * sin(6*chi) +
      (4279*e8/161280) * sin(8*chi);

    do {
      phi = phit;
      phit = M_PI_2 - 2*atan(t*pow((1-e*sin(phi))/(1+e*sin(phi)),e/2));
    } while (fabs(phi-phit) > LOWER_EPS_LIMIT);

    phi = -phi;
    lambda = -(-lambda0 + atan2(x,-y));

    break;

  default:

    /* Now take care of the UTM locations */

    k0 = 0.9996;

    /* Remove false eastings/northings */

    x = utm->x - 500000;
    y = utm->y;

    if (zone_lat > 'B' && zone_lat < 'N')   /* southern hemisphere */
      y -= 10000000;

    /* Calculate the footpoint latitude */

    phi0 = 0.0;
    e1 = (1 - sqrt(1-e2))/(1 + sqrt(1-e2));
    e12 = e1*e1;
    e13 = e1*e12;
    e14 = e12*e12;

    mm0 = a * ((1-e2/4 - 3*e4/64 - 5*e6/256) * phi0 -
               (3*e2/8 + 3*e4/32 + 45*e6/1024) * sin(2*phi0) +
               (15*e4/256 + 45*e6/1024) * sin(4*phi0) -
               (35*e6/3072) * sin(6*phi0));
    mm = mm0 + y/k0;
    mu = mm/(a*(1-e2/4-3*e4/64-5*e6/256));

    phi1 = mu + (3*e1/2 - 27*e13/32) * sin(2*mu) +
      (21*e12/16 - 55*e14/32) * sin(4*mu) +
      (151*e13/96) * sin(6*mu) +
      (1097*e14/512) * sin(8*mu);

    /* Now calculate lambda and phi */

    ep2 = e2/(1-e2);
    cc1 = ep2*cos(phi1)*cos(phi1);
    tt1 = tan(phi1)*tan(phi1);
    nn1 = a / sqrt(1-e2*sin(phi1)*sin(phi1));
    rr1 = a * (1-e2)/pow(1-e2*sin(phi1)*sin(phi1), 1.5);
    dd = x / (nn1 * k0);

    dd2 = dd*dd;
    dd3 = dd*dd2;
    dd4 = dd2*dd2;
    dd5 = dd3*dd2;
    dd6 = dd4*dd2;

    phi = phi1 - (nn1*tan(phi1)/rr1) *
      (dd2/2 - (5+3*tt1+10*cc1-4*cc1*cc1-9*ep2)*dd4/24 +
       (61+90*tt1+298*cc1+45*tt1*tt1-252*ep2-3*cc1*cc1)*dd6/720);
    lambda = lambda0 +
      (dd - (1+2*tt1+cc1)*dd3/6 +
       (5-2*cc1+28*tt1-3*cc1*cc1+8*ep2+24*tt1*tt1)*dd5/120)/ cos(phi1);
  }

  /* Convert phi/lambda to degrees */
  
  ll->latitude = phi * 180.0 / M_PI;
  ll->longitude = lambda * 180.0 / M_PI;
  
  /* All done */

  return(0);
}
