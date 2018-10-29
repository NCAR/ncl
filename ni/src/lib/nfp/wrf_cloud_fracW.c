#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <ncarg/ncl/NclBuiltIns.h>
#include "wrapper.h"

extern void NGCALLF(dcloudfrac2,DCLOUDFRAC2)(double *, double *,
		                                     int *, double *, double *,
		                                     double *, double *,
		                                     double *, double *, double *,
		                                     int *, int *, int *);

extern NclDimRec *get_wrf_dim_info(int,int,int,ng_size_t*);

NhlErrorTypes wrf_cloud_frac_W(void) {
    /* Input parameters */
    void *vert, *rh, *low_thresh, *mid_thresh, *high_thresh, *msg;
    int *vert_inc_w_height;
    double *tmp_vert, *tmp_rh, *tmp_low_thresh, *tmp_mid_thresh, *tmp_high_thresh, *tmp_msg;
    ng_size_t nc, nz, ns, ew, nsew, nznsew, ncnsew;
    int inz, ins, iew;
    ng_size_t dsizes_vert[NCL_MAX_DIMENSIONS], dsizes_rh[NCL_MAX_DIMENSIONS];
    int ndims_vert, ndims_rh;
    NclBasicDataTypes type_vert, type_rh, type_vert_inc, type_low_thresh;
    NclBasicDataTypes type_mid_thresh, type_high_thresh, type_msg;

    /* Output array */
    void *output;
    double *tmp_output;
    int output_ndims;
    long output_dsizes[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_output;
    NclObjClass type_obj_output;

    /* Various. */
    ng_size_t i, index_vertrh, size_output, index_output, size_leftmost;
    double *tmp_low, *tmp_mid, *tmp_high;
    NclQuark *description, *units;
    char *cdescription, *cunits;

    /*
     * Variable for getting/setting dimension name info.
     */
     NclDimRec *dim_info = NULL;
     NclDimRec *dim_info_rh = NULL;

    /*
     * Variables for returning the output array with attributes and/or
     * dimension names attached.
     */
     NclMultiDValData return_md;
     NclVar tmp_var;
     NclStackEntry return_data;
     NclScalar missing_cfrac;


    vert = (void*) NclGetArgValue(
        0,
        7,
        &ndims_vert,
        dsizes_vert,
        NULL,
        NULL,
        &type_vert,
        DONT_CARE);

    rh = (void*) NclGetArgValue(
        1,
        7,
        &ndims_rh,
        dsizes_rh,
        NULL,
        NULL,
        &type_rh,
        DONT_CARE);

    vert_inc_w_height = (int*) NclGetArgValue(
		2,
		7,
		NULL,
		NULL,
		NULL,
		NULL,
		&type_vert_inc,
		DONT_CARE);

    low_thresh = (void*) NclGetArgValue(
		3,
		7,
		NULL,
	    NULL,
	    NULL,
	    NULL,
		&type_low_thresh,
	    DONT_CARE);

    tmp_low_thresh = coerce_input_double(low_thresh,type_low_thresh,1,0,NULL,NULL);

    mid_thresh = (void*) NclGetArgValue(
		4,
		7,
		NULL,
	    NULL,
	    NULL,
	    NULL,
		&type_mid_thresh,
	    DONT_CARE);

    tmp_mid_thresh = coerce_input_double(mid_thresh,type_mid_thresh,1,0,NULL,NULL);

    high_thresh = (void*) NclGetArgValue(
		5,
		7,
		NULL,
	    NULL,
	    NULL,
	    NULL,
		&type_high_thresh,
	    DONT_CARE);

    tmp_high_thresh = coerce_input_double(high_thresh,type_high_thresh,1,0,NULL,NULL);

    msg = (void*) NclGetArgValue(
		6,
		7,
		NULL,
	    NULL,
	    NULL,
	    NULL,
		&type_msg,
	    DONT_CARE);

    tmp_msg = coerce_input_double(msg,type_msg,1,0,NULL,NULL);

    /* Store the dimensions individually for easier access */
    nc   = 3;                           /* cloud level */
    nz   = dsizes_vert[ndims_vert-3];   /* height */
    ns   = dsizes_vert[ndims_vert-2];   /* latitude */
    ew   = dsizes_vert[ndims_vert-1];   /* longitude */
    nsew = ns * ew;
    nznsew = nz * nsew;
    ncnsew = nc * nsew;


    /* Some error checking for our input dimensions */
    if(nz != dsizes_rh[ndims_vert-3] || ns != dsizes_rh[ndims_vert-2] || ew != dsizes_rh[ndims_vert-1]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "wrf_cloud_fraction: rh must be the same dimensionality as vert");
        return(NhlFATAL);
    }

    if(nz > INT_MAX || ns > INT_MAX || ew > INT_MAX) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "wrf_cloud_fraction: one or more of the input dimensions exceeds INT_MAX");
        return(NhlFATAL);
    }
    inz = (int) nz;
    ins = (int) ns;
    iew = (int) ew;


    /* Compute size of leftmost dimensions. */
    size_leftmost = 1;
    for( i = 0; i < ndims_vert-3; i++ ) size_leftmost *= dsizes_vert[i];
    size_output = size_leftmost * ncnsew;

    if(type_msg == NCL_double) {
      missing_cfrac.doubleval = (double) *tmp_msg;
    }
    else {
      missing_cfrac.floatval = (float) *tmp_msg;
    }


    /* 
     * Allocate space for coercing input arrays.  If any of the input
     * is already double, then we don't need to allocate space for
     * temporary arrays, because we'll just change the pointer into
     * the void array appropriately.
     */
    if(type_vert != NCL_double) {
        tmp_vert = (double*)calloc(nznsew,sizeof(double));
        if(tmp_vert == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for coercing vert to double");
            return(NhlFATAL);
        }
    }
    if(type_rh != NCL_double) {
        tmp_rh = (double*)calloc(nznsew,sizeof(double));
        if(tmp_rh == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for coercing rh to double");
        return(NhlFATAL);
        }
    }

    /* Allocate space for output array. */
    if(type_vert == NCL_double || type_rh == NCL_double) {
        output = (void *)calloc(size_output, sizeof(double));
        if(output == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_cloud_fraction: Unable to allocate memory for output array");
            return(NhlFATAL);
        }
        type_output = NCL_double;
        type_obj_output = nclTypedoubleClass;
    } else {
        output     = (void *)calloc(size_output, sizeof(float));
        tmp_output = (double *)calloc(ncnsew,sizeof(double));
        if(output == NULL || tmp_output == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_cloud_fraction: Unable to allocate memory for output array");
            return(NhlFATAL);
        }
        type_output = NCL_float;
        type_obj_output = nclTypefloatClass;
    }

    index_vertrh = 0;
    index_output = 0;
    for( i = 0; i < size_leftmost; i++ ) {
        if(type_vert != NCL_double) {
            coerce_subset_input_double(vert,tmp_vert,index_vertrh,type_vert,nznsew,0,NULL,NULL);
        } else {
            tmp_vert = &((double*)vert)[index_vertrh];
        }

        if(type_rh != NCL_double) {
            coerce_subset_input_double(rh,tmp_rh,index_vertrh,type_rh,nznsew,0,NULL,NULL);
        } else {
            tmp_rh = &((double*)rh)[index_vertrh];
        }

        if(type_output == NCL_double) {
            tmp_low  = &((double*)output)[index_output];
            tmp_mid  = &((double*)output)[index_output+(  size_leftmost*nsew)];
            tmp_high = &((double*)output)[index_output+(2*size_leftmost*nsew)];
        } else {
            tmp_low  = &tmp_output[0];
            tmp_mid  = &tmp_output[  nsew];
            tmp_high = &tmp_output[2*nsew];
        }

        /* Call the Fortran routine */
        NGCALLF(dcloudfrac2,DCLOUDFRAC2)(
            tmp_vert,
            tmp_rh,
			vert_inc_w_height,
			tmp_low_thresh,
			tmp_mid_thresh,
			tmp_high_thresh,
			tmp_msg,
            tmp_low,
            tmp_mid,
            tmp_high,
            &inz,
            &ins,
            &iew);

        /* Coerce output as necessary */
        if(type_output == NCL_float) {
	  /*            coerce_output_float_only(output,tmp_output,ncnsew,index_output);*/
            coerce_output_float_only(output,tmp_low, nsew,index_output);
            coerce_output_float_only(output,tmp_mid, nsew,index_output+(  size_leftmost*nsew));
            coerce_output_float_only(output,tmp_high,nsew,index_output+(2*size_leftmost*nsew));
        }

        index_vertrh += nznsew;
        index_output += nsew;
    }

    /* Free unneeded memory. */
    if(type_vert   != NCL_double) NclFree(tmp_vert);
    if(type_rh     != NCL_double) NclFree(tmp_rh);
    if(type_output != NCL_double) NclFree(tmp_output);
    NclFree(tmp_low_thresh);
    NclFree(tmp_mid_thresh);
    NclFree(tmp_high_thresh);
    NclFree(tmp_msg);


	/*
	* Set up some attributes ("description" and "units") to return.

	*/
	cdescription = (char *)calloc(22,sizeof(char));
	strcpy(cdescription,"Low, Mid, High Clouds");
	cunits       = (char *)calloc(2,sizeof(char));
	strcpy(cunits,"%");
	description = (NclQuark*)NclMalloc(sizeof(NclQuark));
	units       = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*description = NrmStringToQuark(cdescription);
	*units       = NrmStringToQuark(cunits);
	free(cdescription);
	free(cunits);

    /*
     * Return the output array back to NCL. First define the output
     * dimension sizes and the type.
     */
    output_ndims     = ndims_rh;
    output_dsizes[0] = nc;
    for(i = 1; i <= ndims_rh-3; i++) output_dsizes[i] = dsizes_rh[i-1];
    output_dsizes[ndims_rh-2] = ns;    /* lat dimension */
    output_dsizes[ndims_rh-1] = ew;    /* lon dimension */

    /*
	* Get dimension info to see if we have named dimensions.
	* This will be used for return variable.
	*/
	dim_info_rh = get_wrf_dim_info(1,7,ndims_rh,dsizes_rh);
	if(dim_info_rh != NULL) {
	  dim_info = malloc(sizeof(NclDimRec)*output_ndims);
	  if(dim_info == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_cfrac: Unable to allocate memory for holding dimension information");
		return(NhlFATAL);
	  }
	  for(i = 0; i < output_ndims; i++ ) {
		dim_info[i].dim_num  = i;
		dim_info[i].dim_size = output_dsizes[i];
	  }
	  dim_info[0].dim_quark = NrmStringToQuark("low_mid_high");
	  for(i = 0; i < ndims_rh-3; i++) {
		dim_info[i+1].dim_quark = dim_info_rh[i].dim_quark;
	  }
	  dim_info[output_ndims-2].dim_quark = dim_info_rh[ndims_vert-2].dim_quark;
	  dim_info[output_ndims-1].dim_quark = dim_info_rh[ndims_vert-1].dim_quark;

	}

	/*
	 * Set up return value.
	 */
	  return_md = _NclCreateVal(
	                            NULL,
	                            NULL,
	                            Ncl_MultiDValData,
	                            0,
	                            (void*)output,
	                            &missing_cfrac,
	                            output_ndims,
	                            output_dsizes,
	                            TEMPORARY,
	                            NULL,
	                            type_obj_output
	                            );
	  tmp_var = _NclVarCreate(
	                          NULL,
	                          NULL,
	                          Ncl_Var,
	                          0,
	                          NULL,
	                          return_md,
	                          dim_info,
	                          -1,
	                          NULL,
	                          RETURNVAR,
	                          NULL,
	                          TEMPORARY
	                          );

	  if(dim_info   != NULL) NclFree(dim_info);
	  if (dim_info_rh != NULL) NclFree(dim_info_rh);

	/*
	 * Return output grid and attributes to NCL.
	 */
	  return_data.kind = NclStk_VAR;
	  return_data.u.data_var = tmp_var;
	  _NclPlaceReturn(return_data);

	  return(NhlNOERROR);


    /*ret = NclReturnValue(output, output_ndims, output_dsizes, NULL,
                         type_output, 0);
    return(ret);*/
}


