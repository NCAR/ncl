#include <string.h>
#include <stdio.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/color.h>
#include "wrapper.h"

NhlErrorTypes NhlGetNamedColorIndex_W( void )
{
  int i, j, ii, *ci, *wks, nid, total_cname_elements, total_wks_elements;
  NclHLUObj tmp_hlu_obj;
  NrmQuark *cname;
  int ndims_cname;
  ng_size_t dsizes_cname[NCL_MAX_DIMENSIONS];
  int ndims_wks;
  ng_size_t dsizes_wks[NCL_MAX_DIMENSIONS];
  int ndims_out;
  ng_size_t dsizes_out[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 *
 * Retrieve argument #1
 */
  wks   = (int*)NclGetArgValue(0,2,&ndims_wks,dsizes_wks,NULL,NULL,NULL,
                               DONT_CARE);

/*
 * Retrieve argument #2
 */
  cname = (NrmQuark *) NclGetArgValue(1,2,&ndims_cname,dsizes_cname,NULL,NULL,
						NULL,DONT_CARE);
/*
 * Compute total number of elements in wks array.
 */
  total_wks_elements = 1;
  for(i = 0; i < ndims_wks; i++) {
	total_wks_elements *= dsizes_wks[i];
  }
/*
 * Compute total number of elements in color name array.
 */
  total_cname_elements = 1;
  for(i = 0; i < ndims_cname; i++) {
	total_cname_elements *= dsizes_cname[i];
  }
  ci =  (int*)calloc(total_wks_elements*total_cname_elements*sizeof(int),1);
  
  ii = 0;
  for(i = 0; i < total_wks_elements; i++) {
	for(j = 0; j < total_cname_elements; j++) {
/*
 *  Determine the NCL identifier for the graphic object.
 */
	  tmp_hlu_obj = (NclHLUObj) _NclGetObj(wks[i]);
	  nid = tmp_hlu_obj->hlu.hlu_id;
	  ci[ii] = NhlGetNamedColorIndex(nid,NrmQuarkToString(cname[j]));
	  ii++;
	}
  }
/*   
 * If only one workstation has been given, then the number of dimensions
 * of the output is just equal to the dimensions of the colors inputted.
 */
  if(is_scalar(ndims_wks,dsizes_wks)) {
	ndims_out = ndims_cname;
	for( i = 0; i < ndims_cname; i++ ) {
	  dsizes_out[i] = dsizes_cname[i];
	}
  }
  else {
	ndims_out = ndims_cname + ndims_wks;
	for( i = 0; i < ndims_wks; i++ ) {
	  dsizes_out[i] = dsizes_wks[i];
	}
	for( i = 0; i < ndims_cname; i++ ) {
	  dsizes_out[i+ndims_wks] = dsizes_cname[i];
	}
  }
  return(NclReturnValue( (void *) ci, ndims_out, dsizes_out, NULL, NCL_int, 0));
}

NhlErrorTypes rgba_to_color_index_W( void )
{
  int i, *ci;
  float *rgba;
  ng_size_t dsizes[2];
  int ndims;
  int has_alpha;
  int stride;
  int rgba_dim;
  ng_size_t ncolors;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 *
 * Retrieve argument #1
 */

  rgba  = (float *) NclGetArgValue(0,1,&ndims,dsizes,NULL,NULL,
				   NULL,DONT_CARE);

/* ndims must be 1 or 2, dsizes[0] can be any number; dsizes[1] must be 3 or 4 */

  if (ndims == 1) {
	  rgba_dim = 0;
	  ncolors = 1;
  }
  else if (ndims == 2) {
	  rgba_dim = 1;
	  ncolors = dsizes[0];
  }
  else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,
		    "rgba_to_color_index: the input array must have either 1 or 2 dimensions");
	  return(NhlFATAL);
  }
  if (dsizes[rgba_dim] == 3) {
	  has_alpha = 0;
	  stride = 3;
  }
  else if (dsizes[rgba_dim] == 4) {
	  has_alpha = 1;
	  stride = 4;
  }
  else  {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,
		    "rgba_to_color_index: the second dimension of the input array must have either three or four elements");
	  return(NhlFATAL);
  }

  ci =  (int*)calloc(ncolors,sizeof(int));

  for(i = 0; i < ncolors; i++) {
	  ci[i] = _NhlRGBAToColorIndex(rgba + i * stride,has_alpha);
  }

  return(NclReturnValue( (void *) ci, 1, &ncolors, NULL, NCL_int, 0));
}

NhlErrorTypes color_index_to_rgba_W( void )
{
  int i, *ci;
  float *rgba;
  ng_size_t dsizes[2];
  int stride;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 *
 * Retrieve argument #1
 */

  ci  = (int *) NclGetArgValue(0,1,NULL,dsizes,NULL,NULL,
				   NULL,DONT_CARE);

/* ndims must be 1, dsizes[0] can be any number */


  rgba = (float *) calloc(4 * dsizes[0], sizeof(float));

  stride = 4;
  for(i = 0; i < dsizes[0]; i++) {
	  _NhlColorIndexToRGBA(ci[i], rgba + i * stride,1);
  }

  dsizes[1] = 4;

  return(NclReturnValue( (void *) rgba, 2, dsizes, NULL, NCL_float, 0));
}

