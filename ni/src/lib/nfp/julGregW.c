#include <stdio.h>
#include "wrapper.h"

extern double NGCALLF(greg2juld,GREG2JULD)(int *,int *,int *,int *);
extern int    NGCALLF(greg2juli,GREG2JULI)(int *,int *,int *);
extern void   NGCALLF(juld2greg,JULD2GREG)(double *,int *,int *,int *,int *);
extern void   NGCALLF(juli2greg,JULI2GREG)(int *,int *,int *,int *);

NhlErrorTypes greg2jul_W( void )
{
  ng_size_t i, total;
/*
 * Input variables
 */
  int *year, *month, *day, *hour;
  int ndims_year, has_missing_year;
  ng_size_t dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, has_missing_month;
  ng_size_t dsizes_month[NCL_MAX_DIMENSIONS];
  int ndims_day, has_missing_day;
  ng_size_t dsizes_day[NCL_MAX_DIMENSIONS];
  int ndims_hour, has_missing_hour;
  ng_size_t dsizes_hour[NCL_MAX_DIMENSIONS];
  NclScalar missing_year, missing_month, missing_day, missing_hour;
/*
 * Output variables
 */
  int *julian_i, has_missing_julian;
  double *julian_d;
  NclScalar missing_julian;
/*
 * Other variables
 */
  int use_hour;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  year = (int*)NclGetArgValue(
          0,
          4,
          &ndims_year, 
          dsizes_year,
          &missing_year,
          &has_missing_year,
          NULL,
          DONT_CARE);

  month = (int*)NclGetArgValue(
          1,
          4,
          &ndims_month, 
          dsizes_month,
          &missing_month,
          &has_missing_month,
          NULL,
          DONT_CARE);

  day = (int*)NclGetArgValue(
          2,
          4,
          &ndims_day, 
          dsizes_day,
          &missing_day,
          &has_missing_day,
          NULL,
          DONT_CARE);

  hour = (int*)NclGetArgValue(
          3,
          4,
          &ndims_hour,
          dsizes_hour,
          &missing_hour,
          &has_missing_hour,
          NULL,
          DONT_CARE);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_year != ndims_month || ndims_year != ndims_day || 
      ndims_year != ndims_hour) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The input arrays must all be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_year; i++ ) {
    if( dsizes_year[i] != dsizes_day[i] || dsizes_year[i] != dsizes_month[i] ||
                dsizes_year[i] != dsizes_hour[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The input arrays must all have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_year[i];
  }
/*
 * Check input.
 */
  if(has_missing_month || has_missing_day) {
    for( i = 0; i < total; i++ ) {
      if(!(has_missing_month && month[i] == missing_month.intval) && 
         !(1 <= month[i] && month[i] <= 12)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The month values must fall between 1 and 12");
        return(NhlFATAL);
      }
      if(!(has_missing_day && day[i] == missing_day.intval) && 
         !(1 <= day[i] && day[i] <= 31)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The day values must fall between 1 and 31");
        return(NhlFATAL);
      }
    }
  }
  else {
    for( i = 0; i < total; i++ ) {
      if(!(1 <= month[i] && month[i] <= 12)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The month values must fall between 1 and 12");
        return(NhlFATAL);
      }
      if(!(1 <= day[i] && day[i] <= 31)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The day values must fall between 1 and 31");
        return(NhlFATAL);
      }
    }
  }
/*
 * The hour values must all either be negative, indicating they are not to
 * to be used in the calculation, or they must all fall between 0 and 23.
 */
  use_hour = 0;
  if(hour[0] >= 0) use_hour = 1;
  if(use_hour) {
    if(has_missing_hour) {
      for( i = 0; i < total; i++ ) {
        if(!(has_missing_hour && hour[i] == missing_hour.intval) && 
           !(0 <= hour[i] && hour[i] <= 23)) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The hour values must fall between 0 and 23");
          return(NhlFATAL);
        }
      }
    }
    else {
      for( i = 0; i < total; i++ ) {
        if(!(0 <= hour[i] && hour[i] <= 23)) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The hour values must fall between 0 and 23");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(has_missing_hour) {
      for( i = 0; i < total; i++ ) {
        if(!(has_missing_hour && hour[i] == missing_hour.intval) && hour[i] >= 0) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The hour values must all either be negative, or all fall between 0 and 23");
          return(NhlFATAL);
        }
      }
    }
    else {
      for( i = 0; i < total; i++ ) {
        if(hour[i] >= 0) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The hour values must all either be negative, or all fall between 0 and 23");
          return(NhlFATAL);
        }
      }
    }
  }
/*
 * Allocate space for output array.  The output will be of type double if
 * hour values are to be returned, otherwise the output is of type integer.
 */
  if(use_hour) {
    julian_d = (double*)NclMalloc(total*sizeof(double));
    if( julian_d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    julian_i = (int*)NclMalloc(total*sizeof(int));
    if( julian_i == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Check for potential missing values
 */
  if(has_missing_year || has_missing_month || has_missing_day || has_missing_hour) {
    has_missing_julian = 1;
    if(use_hour) {
      missing_julian.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
    else {
      missing_julian.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
    }
  }
  else {
    has_missing_julian = 0;
  }
    
/*
 * Call appropriate function, depending on whether hours are part of the
 * calculation or not.
 */
  if(use_hour) {
    if(has_missing_julian) {
      for( i = 0; i < total; i++ ) {
        if( (has_missing_year  && year[i]  == missing_year.intval) ||
            (has_missing_month && month[i] == missing_month.intval) ||
            (has_missing_day   && day[i]   == missing_day.intval) ||
            (has_missing_hour  && hour[i]  == missing_hour.intval)) {
          julian_d[i] = missing_julian.doubleval;
        }
        else {
          julian_d[i] = NGCALLF(greg2juld,GREG2JULD)(&year[i],&month[i],
                                                     &day[i],&hour[i]);
        }
      }
    }
    else {
      for( i = 0; i < total; i++ ) {
        julian_d[i] = NGCALLF(greg2juld,GREG2JULD)(&year[i],&month[i],
                                                   &day[i],&hour[i]);
      }
    }
    if(has_missing_julian) {
      return(NclReturnValue((void*)julian_d,ndims_year,dsizes_year,
                            &missing_julian,NCL_double,0));
    }
    else {
      return(NclReturnValue((void*)julian_d,ndims_year,dsizes_year,
                            NULL,NCL_double,0));
    }
  }
  else {
    if(has_missing_julian) {
      for( i = 0; i < total; i++ ) {
        if( (has_missing_year  && year[i]  == missing_year.intval) ||
            (has_missing_month && month[i] == missing_month.intval) ||
            (has_missing_day   && day[i]   == missing_day.intval) ||
            (has_missing_hour  && hour[i]  == missing_hour.intval)) {
          julian_i[i] = missing_julian.intval;
        }
        else {
          julian_i[i] = NGCALLF(greg2juli,GREG2JULI)(&year[i],&month[i],&day[i]);
        }
      }
    }
    else {
      for( i = 0; i < total; i++ ) {
        julian_i[i] = NGCALLF(greg2juli,GREG2JULI)(&year[i],&month[i],&day[i]);
      }
    }
    if(has_missing_julian) {
      return(NclReturnValue((void*)julian_i,ndims_year,dsizes_year,
                            &missing_julian,NCL_int,0));
    }
    else {
      return(NclReturnValue((void*)julian_i,ndims_year,dsizes_year,
                            NULL,NCL_int,0));
    }
  }
}


NhlErrorTypes jul2greg_W( void )
{
/*
 * Input variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  NclMultiDValData tmp1_md = NULL;
  int ndims_jul;
/*
 * Output variables
 */
  int *date;
  int ndims_date, has_missing_date;
  ng_size_t dsizes_date[NCL_MAX_DIMENSIONS];
  NclScalar missing_date;
/*
 * Other variables
 */
  ng_size_t i, j, total, num_elems;
  int is_double;
/*
 * Retrieve argument.
 *
 */
  data = _NclGetArg(0,1,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
        tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
        break;
  case NclStk_VAL:
        tmp_md = (NclMultiDValData)data.u.data_obj;
        break;
  default:
        NhlPError(NhlFATAL,NhlEUNKNOWN,"jul2greg: invalid input.");
        return(NhlFATAL);
  }
/*
 * Calculate total size of array.
 */
  total = 1;
  ndims_jul = tmp_md->multidval.n_dims;
  for( i = 0; i < ndims_jul; i++ ) {
        total *= tmp_md->multidval.dim_sizes[i];
  }
/*
 * Allocate space for output array.
 */
  if( tmp_md->multidval.data_type == NCL_double ||
      tmp_md->multidval.data_type == NCL_float) {

/*
 * Promote to double if necessary.
 */
        if(tmp_md->multidval.data_type == NCL_float) {
          tmp1_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
          if(tmp1_md == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"jul2greg: Unable to convert input to double");
                return(NhlFATAL);
          }     
        }
        else {
          tmp1_md = tmp_md;
        }
        is_double = 1;
        num_elems = 4;
  }
  else {
        is_double = 0;
        num_elems = 3;
  }
  date = (int*)NclMalloc(num_elems*total*sizeof(int));
  if( date == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"jul2greg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Check for potential missing values.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    has_missing_date = 1;
    missing_date.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
  }
  else {
    has_missing_date = 0;
  }

/*
 * Call conversion procedure.
 */
  j = 0;
  if(tmp_md->multidval.missing_value.has_missing) {
    if(is_double) {
      for( i = 0; i < total; i++ ) {
        if( ((double*)tmp1_md->multidval.val)[i] == tmp_md->multidval.missing_value.value.doubleval) {
          date[j] = date[j+1] = date[j+2] = date[j+3] = missing_date.intval;
        }
        else {
          NGCALLF(juld2greg,JULD2GREG)(&((double*)tmp1_md->multidval.val)[i],
                                       &date[j],&date[j+1],&date[j+2],&date[j+3]);
        }
        j += num_elems;
      }
    }
    else {
      for( i = 0; i < total; i++ ) {
        if( ((int*)tmp_md->multidval.val)[i] == tmp_md->multidval.missing_value.value.intval) {
          date[j] = date[j+1] = date[j+2] = missing_date.intval;
        }
        else {
          NGCALLF(juli2greg,JULI2GREG)(&((int*)tmp_md->multidval.val)[i],
                                       &date[j],&date[j+1],&date[j+2]);
        }
        j += num_elems;
      }
    }
  }
  else {
    if(is_double) {
      for( i = 0; i < total; i++ ) {
        NGCALLF(juld2greg,JULD2GREG)(&((double*)tmp1_md->multidval.val)[i],
                                     &date[j],&date[j+1],&date[j+2],&date[j+3]);
        j += num_elems;
      }
    }
    else {
      for( i = 0; i < total; i++ ) {
        NGCALLF(juli2greg,JULI2GREG)(&((int*)tmp_md->multidval.val)[i],
                                     &date[j],&date[j+1],&date[j+2]);
        j += num_elems;
      }
    }
  }
/*
 * Return information.
 */
  if(ndims_jul == 1 && tmp_md->multidval.dim_sizes[0] == 1) {
        ndims_date     = 1;
        dsizes_date[0] = num_elems;
  }
  else {
        ndims_date = ndims_jul + 1;
        for( i = 0; i < ndims_jul; i++ ) {
          dsizes_date[i] = tmp_md->multidval.dim_sizes[i];
        }
        dsizes_date[ndims_jul] = num_elems;
  }
  if(has_missing_date) {
    return(NclReturnValue((void*)date,ndims_date,dsizes_date,&missing_date,NCL_int,0));
  }
  else {
    return(NclReturnValue((void*)date,ndims_date,dsizes_date,NULL,NCL_int,0));
  }
}
