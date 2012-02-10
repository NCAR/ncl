#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dcdfbinp,DCDFBINP)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfbins,DCDFBINS)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfbinxn,DCDFBINXN)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfbinpr,DCDFBINPR)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfgamp,DCDFGAMP)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfgamx,DCDFGAMX)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfnorp,DCDFNORP)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfnorx,DCDFNORX)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfchip,DCDFCHIP)(int*, double*, double*, double*, int*);

extern void NGCALLF(dcdfcdftt,DCDFCDFTT)(int *, double *, double *, double* , 
                                         int *);

extern void NGCALLF(dcdfcdftp,DCDFCDFTP)(int *, double *, double *, double* , 
                                         int *);

NhlErrorTypes cdfbin_p_W( void ) {
        void    *s, *xn, *pr, *p;
        int     dummy=0;
        ng_size_t s_dimsizes[NCL_MAX_DIMENSIONS], xn_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t pr_dimsizes[NCL_MAX_DIMENSIONS];
        int s_ndims, xn_ndims, pr_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_s, *tmp_xn, *tmp_pr, *tmp_p;
        NclBasicDataTypes type_s, type_xn, type_pr, type_p;

        /*
        * Retrieve arguments.
        */
        s = (void*) NclGetArgValue(
                0,
                3,
                &s_ndims,
                s_dimsizes,
                NULL,
                NULL,
                &type_s,
                DONT_CARE);

        xn = (void*) NclGetArgValue(
                1,
                3,
                &xn_ndims,
                xn_dimsizes,
                NULL,
                NULL,
                &type_xn,
                DONT_CARE);

        pr = (void*) NclGetArgValue(
                2,
                3,
                &pr_ndims,
                pr_dimsizes,
                NULL,
                NULL,
                &type_pr,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((s_ndims != xn_ndims) || (s_ndims != pr_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_p: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<s_ndims;i++) {
                        if((s_dimsizes[i] != xn_dimsizes[i]) || (s_dimsizes[i] != pr_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_p: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < s_ndims; i++)
                size_x *= s_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_p: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_s = (double *)coerce_input_double(s,type_s, size_x, 0, NULL, NULL);
        if(tmp_s == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_p: Unable to coerce 's' to double");
                return(NhlFATAL);
        }

        tmp_xn = (double *)coerce_input_double(xn,type_xn, size_x, 0, NULL, NULL);
        if(tmp_xn == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_p: Unable to coerce 'xn' to double");
                return(NhlFATAL);
        }

        tmp_pr = (double *)coerce_input_double(pr,type_pr, size_x, 0, NULL, NULL);
        if(tmp_pr == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_p: Unable to coerce 'pr' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_s != NCL_double && type_xn != NCL_double && type_pr != NCL_double) {
                type_p = NCL_float;
                p = (void *) calloc(size_x, sizeof(float));
                tmp_p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL || tmp_p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_p = NCL_double;
                p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_p == NCL_double)
                tmp_p = (double *)p;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfbinp,DCDFBINP)(&isize_x, tmp_s, tmp_xn, tmp_pr, tmp_p, &dummy);

        if(type_p == NCL_float)
                coerce_output_float_only(p,tmp_p,size_x,0);

        /*
        * Free memory.
        */
        if(type_s != NCL_double) NclFree(tmp_s);
        if(type_xn != NCL_double) NclFree(tmp_xn);
        if(type_pr != NCL_double) NclFree(tmp_pr);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(p,s_ndims, s_dimsizes, NULL, type_p, 0));
}


NhlErrorTypes cdfbin_s_W( void ) {
        void    *s, *xn, *pr, *p;
        int     dummy=0;
        ng_size_t xn_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t pr_dimsizes[NCL_MAX_DIMENSIONS], p_dimsizes[NCL_MAX_DIMENSIONS];
        int xn_ndims, pr_ndims, p_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_s, *tmp_xn, *tmp_pr, *tmp_p;
        NclBasicDataTypes type_s, type_xn, type_pr, type_p;

        /*
        * Retrieve arguments.
        */
        p = (void*) NclGetArgValue(
                0,
                3,
                &p_ndims,
                p_dimsizes,
                NULL,
                NULL,
                &type_p,
                DONT_CARE);

        xn = (void*) NclGetArgValue(
                1,
                3,
                &xn_ndims,
                xn_dimsizes,
                NULL,
                NULL,
                &type_xn,
                DONT_CARE);

        pr = (void*) NclGetArgValue(
                2,
                3,
                &pr_ndims,
                pr_dimsizes,
                NULL,
                NULL,
                &type_pr,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((p_ndims != xn_ndims) || (p_ndims != pr_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_s: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<p_ndims;i++) {
                        if((p_dimsizes[i] != xn_dimsizes[i]) || (p_dimsizes[i] != pr_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_s: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < p_ndims; i++)
                size_x *= p_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_s: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
        if(tmp_p == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_s: Unable to coerce 'p' to double");
                return(NhlFATAL);
        }

        tmp_xn = (double *)coerce_input_double(xn,type_xn, size_x, 0, NULL, NULL);
        if(tmp_xn == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_s: Unable to coerce 'xn' to double");
                return(NhlFATAL);
        }

        tmp_pr = (double *)coerce_input_double(pr,type_pr, size_x, 0, NULL, NULL);
        if(tmp_pr == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_s: Unable to coerce 'pr' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_p != NCL_double && type_xn != NCL_double && type_pr != NCL_double) {
                type_s = NCL_float;
                s = (void *) calloc(size_x, sizeof(float));
                tmp_s = (double *) calloc(size_x, sizeof(double));
                if(s == NULL || tmp_s == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_s: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_s = NCL_double;
                s = (double *) calloc(size_x, sizeof(double));
                if(s == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_s: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_s == NCL_double)
                tmp_s = (double *)s;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfbins,DCDFBINS)(&isize_x, tmp_p, tmp_xn, tmp_pr, tmp_s, &dummy);

        if(type_s == NCL_float)
                coerce_output_float_only(s,tmp_s,size_x,0);

        /*
        * Free memory.
        */
        if(type_s != NCL_double) NclFree(tmp_s);
        if(type_xn != NCL_double) NclFree(tmp_xn);
        if(type_pr != NCL_double) NclFree(tmp_pr);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(s,p_ndims, p_dimsizes, NULL, type_s, 0));
}


NhlErrorTypes cdfbin_xn_W( void ) {
        void    *xn, *s, *pr, *p;
        int     dummy=0;
        ng_size_t s_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t pr_dimsizes[NCL_MAX_DIMENSIONS], p_dimsizes[NCL_MAX_DIMENSIONS];
        int s_ndims, pr_ndims, p_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_xn, *tmp_s, *tmp_pr, *tmp_p;
        NclBasicDataTypes type_xn, type_s, type_pr, type_p;

        /*
        * Retrieve arguments.
        */
        p = (void*) NclGetArgValue(
                0,
                3,
                &p_ndims,
                p_dimsizes,
                NULL,
                NULL,
                &type_p,
                DONT_CARE);

        s = (void*) NclGetArgValue(
                1,
                3,
                &s_ndims,
                s_dimsizes,
                NULL,
                NULL,
                &type_s,
                DONT_CARE);

        pr = (void*) NclGetArgValue(
                2,
                3,
                &pr_ndims,
                pr_dimsizes,
                NULL,
                NULL,
                &type_pr,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((p_ndims != s_ndims) || (p_ndims != pr_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_xn: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<p_ndims;i++) {
                        if((p_dimsizes[i] != s_dimsizes[i]) || (p_dimsizes[i] != pr_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_xn: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < p_ndims; i++)
                size_x *= p_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_xn: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
        if(tmp_p == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_xn: Unable to coerce 'p' to double");
                return(NhlFATAL);
        }

        tmp_s = (double *)coerce_input_double(s,type_s, size_x, 0, NULL, NULL);
        if(tmp_s == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_xn: Unable to coerce 's' to double");
                return(NhlFATAL);
        }

        tmp_pr = (double *)coerce_input_double(pr,type_pr, size_x, 0, NULL, NULL);
        if(tmp_pr == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_xn: Unable to coerce 'pr' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_p != NCL_double && type_s != NCL_double && type_pr != NCL_double) {
                type_xn = NCL_float;
                xn = (void *) calloc(size_x, sizeof(float));
                tmp_xn = (double *) calloc(size_x, sizeof(double));
                if(xn == NULL || tmp_xn == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_xn: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_xn = NCL_double;
                xn = (double *) calloc(size_x, sizeof(double));
                if(xn == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_xn: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_xn == NCL_double)
                tmp_xn = (double *)xn;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfbinxn,DCDFBINXN)(&isize_x, tmp_p, tmp_s, tmp_pr, tmp_xn, &dummy);

        if(type_xn == NCL_float)
                coerce_output_float_only(xn,tmp_xn,size_x,0);

        /*
        * Free memory.
        */
        if(type_xn != NCL_double) NclFree(tmp_xn);
        if(type_s != NCL_double) NclFree(tmp_s);
        if(type_pr != NCL_double) NclFree(tmp_pr);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(xn,p_ndims, p_dimsizes, NULL, type_xn, 0));
}


NhlErrorTypes cdfbin_pr_W( void ) {
        void    *pr, *s, *xn, *p;
        int     dummy=0;
        ng_size_t s_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t xn_dimsizes[NCL_MAX_DIMENSIONS], p_dimsizes[NCL_MAX_DIMENSIONS];
        int s_ndims, xn_ndims, p_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_pr, *tmp_s, *tmp_xn, *tmp_p;
        NclBasicDataTypes type_pr, type_s, type_xn, type_p;

        /*
        * Retrieve arguments.
        */
        p = (void*) NclGetArgValue(
                0,
                3,
                &p_ndims,
                p_dimsizes,
                NULL,
                NULL,
                &type_p,
                DONT_CARE);

        s = (void*) NclGetArgValue(
                1,
                3,
                &s_ndims,
                s_dimsizes,
                NULL,
                NULL,
                &type_s,
                DONT_CARE);

        xn = (void*) NclGetArgValue(
                2,
                3,
                &xn_ndims,
                xn_dimsizes,
                NULL,
                NULL,
                &type_xn,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((p_ndims != s_ndims) || (p_ndims != xn_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_pr: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<p_ndims;i++) {
                        if((p_dimsizes[i] != s_dimsizes[i]) || (p_dimsizes[i] != xn_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfbin_pr: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < p_ndims; i++)
                size_x *= p_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_pr: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
        if(tmp_p == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_pr: Unable to coerce 'p' to double");
                return(NhlFATAL);
        }

        tmp_s = (double *)coerce_input_double(s,type_s, size_x, 0, NULL, NULL);
        if(tmp_s == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_pr: Unable to coerce 's' to double");
                return(NhlFATAL);
        }

        tmp_xn = (double *)coerce_input_double(xn,type_xn, size_x, 0, NULL, NULL);
        if(tmp_xn == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_pr: Unable to coerce 'xn' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_p != NCL_double && type_s != NCL_double && type_xn != NCL_double) {
                type_pr = NCL_float;
                pr = (void *) calloc(size_x, sizeof(float));
                tmp_pr = (double *) calloc(size_x, sizeof(double));
                if(pr == NULL || tmp_pr == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_pr: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_pr = NCL_double;
                pr = (double *) calloc(size_x, sizeof(double));
                if(pr == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfbin_pr: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_pr == NCL_double)
                tmp_pr = (double *)pr;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfbinpr,DCDFBINPR)(&isize_x, tmp_p, tmp_s, tmp_xn, tmp_pr, &dummy);

        if(type_pr == NCL_float)
                coerce_output_float_only(pr,tmp_pr,size_x,0);

        /*
        * Free memory.
        */
        if(type_pr != NCL_double) NclFree(tmp_pr);
        if(type_s != NCL_double) NclFree(tmp_s);
        if(type_xn != NCL_double) NclFree(tmp_xn);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(pr,p_ndims, p_dimsizes, NULL, type_pr, 0));
}


NhlErrorTypes cdfgam_p_W( void ) {
        void    *x, *shape, *scale, *p;
        int     dummy=0;
	ng_size_t x_dimsizes[NCL_MAX_DIMENSIONS], shape_dimsizes[NCL_MAX_DIMENSIONS];
	ng_size_t scale_dimsizes[NCL_MAX_DIMENSIONS];
        int x_ndims, shape_ndims, scale_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_x, *tmp_shape, *tmp_scale, *tmp_p;
        NclBasicDataTypes type_x, type_shape, type_scale, type_p;

        /*
        * Retrieve arguments.
        */
        x = (void*) NclGetArgValue(
                0,
                3,
                &x_ndims,
                x_dimsizes,
                NULL,
                NULL,
                &type_x,
                DONT_CARE);

        shape = (void*) NclGetArgValue(
                1,
                3,
                &shape_ndims,
                shape_dimsizes,
                NULL,
                NULL,
                &type_shape,
                DONT_CARE);

        scale = (void*) NclGetArgValue(
                2,
                3,
                &scale_ndims,
                scale_dimsizes,
                NULL,
                NULL,
                &type_scale,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((x_ndims != shape_ndims) || (x_ndims != scale_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfgam_p: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<x_ndims;i++) {
                        if((x_dimsizes[i] != shape_dimsizes[i]) || (x_dimsizes[i] != scale_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfgam_p: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < x_ndims; i++)
                size_x *= x_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_p: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

       /*
        * Coerce input arguments.
        */
        tmp_x = (double *)coerce_input_double(x,type_x, size_x, 0, NULL, NULL);
        if(tmp_x == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_p: Unable to coerce 'x' to double");
                return(NhlFATAL);
        }

        tmp_shape = (double *)coerce_input_double(shape,type_shape, size_x, 0, NULL, NULL);
        if(tmp_shape == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_p: Unable to coerce 'shape' to double");
                return(NhlFATAL);
        }

        tmp_scale = (double *)coerce_input_double(scale,type_scale, size_x, 0, NULL, NULL);
        if(tmp_scale == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_p: Unable to coerce 'scale' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_x != NCL_double && type_shape != NCL_double && type_scale != NCL_double) {
                type_p = NCL_float;
                p = (void *) calloc(size_x, sizeof(float));
                tmp_p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL || tmp_p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_p = NCL_double;
                p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_p == NCL_double)
                tmp_p = (double *)p;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfgamp,DCDFGAMP)(&isize_x, tmp_x, tmp_shape, tmp_scale, tmp_p, &dummy);

        if(type_p == NCL_float)
                coerce_output_float_only(p,tmp_p,size_x,0);

        /*
        * Free memory.
        */
        if(type_x != NCL_double) NclFree(tmp_x);
        if(type_shape != NCL_double) NclFree(tmp_shape);
        if(type_scale != NCL_double) NclFree(tmp_scale);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(p,x_ndims, x_dimsizes, NULL, type_p, 0));
}



NhlErrorTypes cdfgam_x_W( void ) {
        void    *p, *shape, *scale, *x;
        int     dummy=0;
        ng_size_t p_dimsizes[NCL_MAX_DIMENSIONS], shape_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t scale_dimsizes[NCL_MAX_DIMENSIONS];
        int p_ndims, shape_ndims, scale_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_p, *tmp_shape, *tmp_scale, *tmp_x;
        NclBasicDataTypes type_p, type_shape, type_scale, type_x;

        /*
        * Retrieve arguments.
        */
        p = (void*) NclGetArgValue(
                0,
                3,
                &p_ndims,
                p_dimsizes,
                NULL,
                NULL,
                &type_p,
                DONT_CARE);

        shape = (void*) NclGetArgValue(
                1,
                3,
                &shape_ndims,
                shape_dimsizes,
                NULL,
                NULL,
                &type_shape,
                DONT_CARE);

        scale = (void*) NclGetArgValue(
                2,
                3,
                &scale_ndims,
                scale_dimsizes,
                NULL,
                NULL,
                &type_scale,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((p_ndims != shape_ndims) || (p_ndims != scale_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfgam_x: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<p_ndims;i++) {
                        if((p_dimsizes[i] != shape_dimsizes[i]) || (p_dimsizes[i] != scale_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfgam_x: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < p_ndims; i++)
                size_x *= p_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_x: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
        if(tmp_p == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_x: Unable to coerce 'p' to double");
                return(NhlFATAL);
        }

        tmp_shape = (double *)coerce_input_double(shape,type_shape, size_x, 0, NULL, NULL);
        if(tmp_shape == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_x: Unable to coerce 'shape' to double");
                return(NhlFATAL);
        }

        tmp_scale = (double *)coerce_input_double(scale,type_scale, size_x, 0, NULL, NULL);
        if(tmp_scale == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_x: Unable to coerce 'scale' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_p != NCL_double && type_shape != NCL_double && type_scale != NCL_double) {
                type_x = NCL_float;
                x = (void *) calloc(size_x, sizeof(float));
                tmp_x = (double *) calloc(size_x, sizeof(double));
                if(x == NULL || tmp_x == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_x: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_x = NCL_double;
                x = (double *) calloc(size_x, sizeof(double));
                if(x == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfgam_x: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_x == NCL_double)
                tmp_x = (double *)x;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfgamx,DCDFGAMX)(&isize_x, tmp_p, tmp_shape, tmp_scale, tmp_x, &dummy);

        if(type_x == NCL_float)
                coerce_output_float_only(x,tmp_x,size_x,0);

        /*
        * Free memory.
        */
        if(type_p != NCL_double) NclFree(tmp_p);
        if(type_shape != NCL_double) NclFree(tmp_shape);
        if(type_scale != NCL_double) NclFree(tmp_scale);
        if(type_x != NCL_double) NclFree(tmp_x);

        return(NclReturnValue(x,p_ndims, p_dimsizes, NULL, type_x, 0));
}


NhlErrorTypes cdfnor_p_W( void ) {
        void    *x, *mean, *sd, *p;
        int     dummy=0;
        ng_size_t x_dimsizes[NCL_MAX_DIMENSIONS], mean_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t sd_dimsizes[NCL_MAX_DIMENSIONS];
        int x_ndims, mean_ndims, sd_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_x, *tmp_mean, *tmp_sd, *tmp_p;
        NclBasicDataTypes type_x, type_mean, type_sd, type_p;

        /*
        * Retrieve arguments.
        */
        x = (void*) NclGetArgValue(
                0,
                3,
                &x_ndims,
                x_dimsizes,
                NULL,
                NULL,
                &type_x,
                DONT_CARE);

        mean = (void*) NclGetArgValue(
                1,
                3,
                &mean_ndims,
                mean_dimsizes,
                NULL,
                NULL,
                &type_mean,
                DONT_CARE);

        sd = (void*) NclGetArgValue(
                2,
                3,
                &sd_ndims,
                sd_dimsizes,
                NULL,
                NULL,
                &type_sd,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((x_ndims != mean_ndims) || (x_ndims != sd_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfnor_p: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<x_ndims;i++) {
                        if((x_dimsizes[i] != mean_dimsizes[i]) || (x_dimsizes[i] != sd_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfnor_p: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < x_ndims; i++)
                size_x *= x_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_p: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_x = (double *)coerce_input_double(x,type_x, size_x, 0, NULL, NULL);
        if(tmp_x == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_p: Unable to coerce 'x' to double");
                return(NhlFATAL);
        }

        tmp_mean = (double *)coerce_input_double(mean,type_mean, size_x, 0, NULL, NULL);
        if(tmp_mean == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_p: Unable to coerce 'mean' to double");
                return(NhlFATAL);
        }

        tmp_sd = (double *)coerce_input_double(sd,type_sd, size_x, 0, NULL, NULL);
        if(tmp_sd == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_p: Unable to coerce 'sd' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_x != NCL_double && type_mean != NCL_double && type_sd != NCL_double) {
                type_p = NCL_float;
                p = (void *) calloc(size_x, sizeof(float));
                tmp_p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL || tmp_p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_p = NCL_double;
                p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_p == NCL_double)
                tmp_p = (double *)p;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfnorp,DCDFNORP)(&isize_x, tmp_x, tmp_mean, tmp_sd, tmp_p, &dummy);

        if(type_p == NCL_float)
                coerce_output_float_only(p,tmp_p,size_x,0);

        /*
        * Free memory.
        */
        if(type_x != NCL_double) NclFree(tmp_x);
        if(type_mean != NCL_double) NclFree(tmp_mean);
        if(type_sd != NCL_double) NclFree(tmp_sd);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(p,x_ndims, x_dimsizes, NULL, type_p, 0));
}


NhlErrorTypes cdfnor_x_W( void ) {
        void    *p, *mean, *sd, *x;
        int     dummy=0;
        ng_size_t p_dimsizes[NCL_MAX_DIMENSIONS], mean_dimsizes[NCL_MAX_DIMENSIONS];
        ng_size_t sd_dimsizes[NCL_MAX_DIMENSIONS];
        int p_ndims, mean_ndims, sd_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_p, *tmp_mean, *tmp_sd, *tmp_x;
        NclBasicDataTypes type_p, type_mean, type_sd, type_x;

        /*
        * Retrieve arguments.
        */
        p = (void*) NclGetArgValue(
                0,
                3,
                &p_ndims,
                p_dimsizes,
                NULL,
                NULL,
                &type_p,
                DONT_CARE);

        mean = (void*) NclGetArgValue(
                1,
                3,
                &mean_ndims,
                mean_dimsizes,
                NULL,
                NULL,
                &type_mean,
                DONT_CARE);

        sd = (void*) NclGetArgValue(
                2,
                3,
                &sd_ndims,
                sd_dimsizes,
                NULL,
                NULL,
                &type_sd,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if((p_ndims != mean_ndims) || (p_ndims != sd_ndims)) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfnor_x: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<p_ndims;i++) {
                        if((p_dimsizes[i] != mean_dimsizes[i]) || (p_dimsizes[i] != sd_dimsizes[i]) ) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfnor_x: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < p_ndims; i++)
                size_x *= p_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_x: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
        if(tmp_p == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_x: Unable to coerce 'p' to double");
                return(NhlFATAL);
        }

        tmp_mean = (double *)coerce_input_double(mean,type_mean, size_x, 0, NULL, NULL);
        if(tmp_mean == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_x: Unable to coerce 'mean' to double");
                return(NhlFATAL);
        }

        tmp_sd = (double *)coerce_input_double(sd,type_sd, size_x, 0, NULL, NULL);
        if(tmp_sd == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_x: Unable to coerce 'sd' to double");
                return(NhlFATAL);
        }

        /*
        * Allocate space for output array.
        */
        if(type_p != NCL_double && type_mean != NCL_double && type_sd != NCL_double) {
                type_x = NCL_float;
                x = (void *) calloc(size_x, sizeof(float));
                tmp_x = (double *) calloc(size_x, sizeof(double));
                if(x == NULL || tmp_x == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_x: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_x = NCL_double;
                x = (double *) calloc(size_x, sizeof(double));
                if(x == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfnor_x: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_x == NCL_double)
                tmp_x = (double *)x;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfnorx,DCDFNORX)(&isize_x, tmp_p, tmp_mean, tmp_sd, tmp_x, &dummy);

        if(type_x == NCL_float)
                coerce_output_float_only(x,tmp_x,size_x,0);

        /*
        * Free memory.
        */
        if(type_p != NCL_double) NclFree(tmp_p);
        if(type_mean != NCL_double) NclFree(tmp_mean);
        if(type_sd != NCL_double) NclFree(tmp_sd);
        if(type_x != NCL_double) NclFree(tmp_x);

        return(NclReturnValue(x,p_ndims, p_dimsizes, NULL, type_x, 0));
}


NhlErrorTypes cdfchi_p_W( void ) {
        void    *x, *df, *p;
        int     dummy=0;
        ng_size_t x_dimsizes[NCL_MAX_DIMENSIONS], df_dimsizes[NCL_MAX_DIMENSIONS];
        int x_ndims, df_ndims;

        /* Declaring temporary variables */

        int i, isize_x;
	ng_size_t size_x;
        double *tmp_x, *tmp_df, *tmp_p;
        NclBasicDataTypes type_x, type_df, type_p;

        /*
        * Retrieve arguments.
        */
        x = (void*) NclGetArgValue(
                0,
                2,
                &x_ndims,
                x_dimsizes,
                NULL,
                NULL,
                &type_x,
                DONT_CARE);

        df = (void*) NclGetArgValue(
                1,
                2,
                &df_ndims,
                df_dimsizes,
                NULL,
                NULL,
                &type_df,
                DONT_CARE);


        /*
        * Make sure all of the input arguments are of equal dimensions.
        */
        if(x_ndims != df_ndims) {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfchi_p: The input arrays must have the same number of dimensions");
                return(NhlFATAL);
        }
        else {
                for(i=0;i<x_ndims;i++) {
                        if(x_dimsizes[i] != df_dimsizes[i]) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN, "cdfchi_p: The input arrays must have the same dimension sizes");
                                return(NhlFATAL);
                        }
                }
        }
        
        /*
        * Compute the total size of the output array.
        */
        size_x = 1;
        for(i=0; i < x_ndims; i++)
                size_x *= x_dimsizes[i];

/*
 * Test input dimension size to make sure it is <= INT_MAX.
 */
	if(size_x > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfchi_p: size_x = %ld is larger than INT_MAX", size_x);
	  return(NhlFATAL);
	}
	isize_x = (int) size_x;

        /*
        * Coerce input arguments.
        */
        tmp_x = (double *)coerce_input_double(x,type_x, size_x, 0, NULL, NULL);
        if(tmp_x == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfchi_p: Unable to coerce 'x' to double");
                return(NhlFATAL);
        }

        tmp_df = (double *)coerce_input_double(df,type_df, size_x, 0, NULL, NULL);
        if(tmp_df == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfchi_p: Unable to coerce 'df' to double");
                return(NhlFATAL);
        }


        /*
        * Allocate space for output array.
        */
        if(type_x != NCL_double && type_df != NCL_double) {
                type_p = NCL_float;
                p = (void *) calloc(size_x, sizeof(float));
                tmp_p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL || tmp_p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfchi_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }
        else {
                type_p = NCL_double;
                p = (double *) calloc(size_x, sizeof(double));
                if(p == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"cdfchi_p: Unable to allocate memory for output array");
                        return(NhlFATAL);
                }
        }


        if(type_p == NCL_double)
                tmp_p = (double *)p;
                
        /*
        * Call the Fortran version of this routine.
        */
	NGCALLF(dcdfchip,DCDFCHIP)(&isize_x, tmp_x, tmp_df, tmp_p, &dummy);

        if(type_p == NCL_float)
                coerce_output_float_only(p,tmp_p,size_x,0);

        /*
        * Free memory.
        */
        if(type_x != NCL_double) NclFree(tmp_x);
        if(type_df != NCL_double) NclFree(tmp_df);
        if(type_p != NCL_double) NclFree(tmp_p);

        return(NclReturnValue(p,x_ndims, x_dimsizes, NULL, type_p, 0));
}

NhlErrorTypes cdft_t_W( void )
{
/*
 * Input variables.
 */
  void  *p, *df;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_df;
  ng_size_t dsizes_df[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_df;
  double *tmp_p = NULL;
  double *tmp_df = NULL;
/*
 * Output variables.
 */
  void *t;
  double *tmp_t = NULL;
  NclBasicDataTypes type_t;
/*
 * Various
 */
  int n;
  ng_size_t i, index_p;
  ng_size_t size_leftmost, size_t;
  int ier;
 
/*
 * Retrieve arguments.
 */
  p = (void*) NclGetArgValue(
             0,
             2,
             &ndims_p,
             dsizes_p,
             NULL,
             NULL,
             &type_p,
             DONT_CARE);

  df = (void*) NclGetArgValue(
             1,
             2,
             &ndims_df,
             dsizes_df,
             NULL,
             NULL,
             &type_df,
             DONT_CARE);

/*
 * Make sure all of the input arguments are of equal dimensions.
 */
  if(ndims_p != ndims_df) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "cdft_t: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_p; i++) {
    if(dsizes_p[i] != dsizes_df[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN, "cdft_t: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
        
/*
 * Compute leftmost and total dimension sizes.
 */
  n             = dsizes_p[ndims_p-1];
  size_leftmost = 1;
  for(i = 0; i < ndims_p-1; i++) {
    size_leftmost *= dsizes_p[i];
  }
  size_t = size_leftmost * n;

/*
 * Create temporary arrays for input.
 */
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(n,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_t: Unable to coerce 'p' to double");
      return(NhlFATAL);
    }
  }    
  if(type_df != NCL_double) {
    tmp_df = (double *)calloc(n,sizeof(double));
    if(tmp_df == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_t: Unable to coerce 'p' to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_p != NCL_double && type_df != NCL_double) {
    type_t = NCL_float;
    t     = (void *) calloc(size_t, sizeof(float));
    tmp_t = (double *) calloc(n, sizeof(double));
    if(t == NULL || tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_t: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_t = NCL_double;
    t = (double *) calloc(size_t, sizeof(double));
    if(t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_t: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  index_p = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_p != NCL_double) {
/*
 * Coerce subsection of p (tmp_p) to double.
 */
      coerce_subset_input_double(p,tmp_p,index_p,type_p,n,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[index_p];
    }

    if(type_df != NCL_double) {
/*
 * Coerce subsection of df (tmp_df) to double.
 */
      coerce_subset_input_double(df,tmp_df,index_p,type_df,n,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_df to appropriate location in df.
 */
      tmp_df = &((double*)df)[index_p];
    }

    if(type_t == NCL_double) tmp_t = &((double*)t)[index_p];

    NGCALLF(dcdfcdftt,DCDFCDFTT)(&n, tmp_p, tmp_t, tmp_df, &ier);

    if(type_t == NCL_float) {
      coerce_output_float_only(t,tmp_t,n,index_p);
    }
    index_p += n;
  }

/*
 * Free memory.
 */
  if(type_t != NCL_double) NclFree(tmp_t);
  if(type_p != NCL_double) NclFree(tmp_p);
  if(type_df != NCL_double) NclFree(tmp_df);

  return(NclReturnValue(t, ndims_p, dsizes_p, NULL, type_t, 0));
}

NhlErrorTypes cdft_p_W( void )
{
/*
 * Input variables.
 */
  void  *t, *df;
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_df;
  ng_size_t dsizes_df[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_df;
  double *tmp_t = NULL;
  double *tmp_df = NULL;
/*
 * Output variables.
 */
  void *p;
  double *tmp_p = NULL;
  NclBasicDataTypes type_p;
/*
 * Various
 */
  int n;
  ng_size_t i, index_t;
  ng_size_t size_leftmost, size_p;
  int ier;
 
/*
 * Retrieve arguments.
 */
  t = (void*) NclGetArgValue(
             0,
             2,
             &ndims_t,
             dsizes_t,
             NULL,
             NULL,
             &type_t,
             DONT_CARE);

  df = (void*) NclGetArgValue(
             1,
             2,
             &ndims_df,
             dsizes_df,
             NULL,
             NULL,
             &type_df,
             DONT_CARE);

/*
 * Make sure all of the input arguments are of equal dimensions.
 */
  if(ndims_t != ndims_df) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "cdft_t: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_t; i++) {
    if(dsizes_t[i] != dsizes_df[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN, "cdft_p: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
        
/*
 * Compute leftmost and total dimension sizes.
 */
  n             = dsizes_t[ndims_t-1];
  size_leftmost = 1;
  for(i = 0; i < ndims_t-1; i++) {
    size_leftmost *= dsizes_t[i];
  }
  size_p = size_leftmost * n;

/*
 * Create temporary arrays for input.
 */
  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(n,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_p: Unable to coerce 't' to double");
      return(NhlFATAL);
    }
  }    
  if(type_df != NCL_double) {
    tmp_df = (double *)calloc(n,sizeof(double));
    if(tmp_df == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_p: Unable to coerce 'p' to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_t != NCL_double && type_df != NCL_double) {
    type_p = NCL_float;
    p      = (void *) calloc(size_p, sizeof(float));
    tmp_p  = (double *) calloc(n, sizeof(double));
    if(p == NULL || tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_p: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_p = NCL_double;
    p = (double *) calloc(size_p, sizeof(double));
    if(p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cdft_p: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  index_t = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_t != NCL_double) {
/*
` * Coerce subsection of t (tmp_t) to double.
 */
      coerce_subset_input_double(t,tmp_t,index_t,type_t,n,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[index_t];
    }

    if(type_df != NCL_double) {
/*
 * Coerce subsection of df (tmp_df) to double.
 */
      coerce_subset_input_double(df,tmp_df,index_t,type_df,n,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_df to appropriate location in df.
 */
      tmp_df = &((double*)df)[index_t];
    }

    if(type_p == NCL_double) tmp_p = &((double*)p)[index_t];

    NGCALLF(dcdfcdftp,DCDFCDFTP)(&n, tmp_p, tmp_t, tmp_df, &ier);

    if(type_p == NCL_float) {
      coerce_output_float_only(p,tmp_p,n,index_t);
    }
    index_t += n;
  }

/*
 * Free memory.
 */
  if(type_t != NCL_double) NclFree(tmp_t);
  if(type_p != NCL_double) NclFree(tmp_p);
  if(type_df != NCL_double) NclFree(tmp_df);

  return(NclReturnValue(p, ndims_t, dsizes_t, NULL, type_p, 0));
}
