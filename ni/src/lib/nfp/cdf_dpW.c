#include<stdio.h>

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

extern void NGCALLF(dcdfbinp,DCDFBINP)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfbins,DCDFBINS)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfbinxn,DCDFBINXN)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfbinpr,DCDFBINPR)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfgamp,DCDFGAMP)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfgamx,DCDFGAMX)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfnorp,DCDFNORP)(int*, double*, double*, double*, double*, int*);
extern void NGCALLF(dcdfnorx,DCDFNORX)(int*, double*, double*, double*, double*, int*);

NhlErrorTypes dcdfbinp_W( void ) {
	void	*s, *xn, *pr, *p;
	int	dummy=0;
	int s_dimsizes[NCL_MAX_DIMENSIONS], xn_dimsizes[NCL_MAX_DIMENSIONS];
	int pr_dimsizes[NCL_MAX_DIMENSIONS];
	int s_ndims, xn_ndims, pr_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	xn = (void*) NclGetArgValue(
		1,
		3,
		&xn_ndims,
		xn_dimsizes,
		NULL,
		NULL,
		&type_xn,
		2);

	pr = (void*) NclGetArgValue(
		2,
		3,
		&pr_ndims,
		pr_dimsizes,
		NULL,
		NULL,
		&type_pr,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((s_ndims != xn_ndims) || (s_ndims != pr_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinp: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<s_ndims;i++) {
			if((s_dimsizes[i] != xn_dimsizes[i]) || (s_dimsizes[i] != pr_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinp: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_s = (double *)coerce_input_double(s,type_s, size_x, 0, NULL, NULL);
	if(tmp_s == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinp: Unable to coerce 's' to double");
		return(NhlFATAL);
	}

	tmp_xn = (double *)coerce_input_double(xn,type_xn, size_x, 0, NULL, NULL);
	if(tmp_xn == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinp: Unable to coerce 'xn' to double");
		return(NhlFATAL);
	}

	tmp_pr = (double *)coerce_input_double(pr,type_pr, size_x, 0, NULL, NULL);
	if(tmp_pr == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinp: Unable to coerce 'pr' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinp: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_p = NCL_double;
		p = (double *) calloc(size_x, sizeof(double));
		if(p == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinp: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_p == NCL_double)
		tmp_p = (double *)p;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfbinp,DCDFBINP)(&size_x, tmp_s, tmp_xn, tmp_pr, tmp_p, &dummy);

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


NhlErrorTypes dcdfbinx_W( void ) {
	void	*s, *xn, *pr, *p;
	int	dummy=0;
	int xn_dimsizes[NCL_MAX_DIMENSIONS];
	int pr_dimsizes[NCL_MAX_DIMENSIONS], p_dimsizes[NCL_MAX_DIMENSIONS];
	int xn_ndims, pr_ndims, p_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	xn = (void*) NclGetArgValue(
		1,
		3,
		&xn_ndims,
		xn_dimsizes,
		NULL,
		NULL,
		&type_xn,
		2);

	pr = (void*) NclGetArgValue(
		2,
		3,
		&pr_ndims,
		pr_dimsizes,
		NULL,
		NULL,
		&type_pr,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((p_ndims != xn_ndims) || (p_ndims != pr_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinx: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<p_ndims;i++) {
			if((p_dimsizes[i] != xn_dimsizes[i]) || (p_dimsizes[i] != pr_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinx: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
	if(tmp_p == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinx: Unable to coerce 'p' to double");
		return(NhlFATAL);
	}

	tmp_xn = (double *)coerce_input_double(xn,type_xn, size_x, 0, NULL, NULL);
	if(tmp_xn == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinx: Unable to coerce 'xn' to double");
		return(NhlFATAL);
	}

	tmp_pr = (double *)coerce_input_double(pr,type_pr, size_x, 0, NULL, NULL);
	if(tmp_pr == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinx: Unable to coerce 'pr' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinx: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_s = NCL_double;
		s = (double *) calloc(size_x, sizeof(double));
		if(s == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinx: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_s == NCL_double)
		tmp_s = (double *)s;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfbins,DCDFBINS)(&size_x, tmp_p, tmp_xn, tmp_pr, tmp_s, &dummy);

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


NhlErrorTypes dcdfbinxn_W( void ) {
	void	*xn, *s, *pr, *p;
	int	dummy=0;
	int s_dimsizes[NCL_MAX_DIMENSIONS];
	int pr_dimsizes[NCL_MAX_DIMENSIONS], p_dimsizes[NCL_MAX_DIMENSIONS];
	int s_ndims, pr_ndims, p_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	s = (void*) NclGetArgValue(
		1,
		3,
		&s_ndims,
		s_dimsizes,
		NULL,
		NULL,
		&type_s,
		2);

	pr = (void*) NclGetArgValue(
		2,
		3,
		&pr_ndims,
		pr_dimsizes,
		NULL,
		NULL,
		&type_pr,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((p_ndims != s_ndims) || (p_ndims != pr_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinxn: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<p_ndims;i++) {
			if((p_dimsizes[i] != s_dimsizes[i]) || (p_dimsizes[i] != pr_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinxn: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
	if(tmp_p == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinxn: Unable to coerce 'p' to double");
		return(NhlFATAL);
	}

	tmp_s = (double *)coerce_input_double(s,type_s, size_x, 0, NULL, NULL);
	if(tmp_s == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinxn: Unable to coerce 's' to double");
		return(NhlFATAL);
	}

	tmp_pr = (double *)coerce_input_double(pr,type_pr, size_x, 0, NULL, NULL);
	if(tmp_pr == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinxn: Unable to coerce 'pr' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinxn: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_xn = NCL_double;
		xn = (double *) calloc(size_x, sizeof(double));
		if(xn == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinxn: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_xn == NCL_double)
		tmp_xn = (double *)xn;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfbinxn,DCDFBINXN)(&size_x, tmp_p, tmp_s, tmp_pr, tmp_xn, &dummy);

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


NhlErrorTypes dcdfbinpr_W( void ) {
	void	*pr, *s, *xn, *p;
	int	dummy=0;
	int s_dimsizes[NCL_MAX_DIMENSIONS];
	int xn_dimsizes[NCL_MAX_DIMENSIONS], p_dimsizes[NCL_MAX_DIMENSIONS];
	int s_ndims, xn_ndims, p_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	s = (void*) NclGetArgValue(
		1,
		3,
		&s_ndims,
		s_dimsizes,
		NULL,
		NULL,
		&type_s,
		2);

	xn = (void*) NclGetArgValue(
		2,
		3,
		&xn_ndims,
		xn_dimsizes,
		NULL,
		NULL,
		&type_xn,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((p_ndims != s_ndims) || (p_ndims != xn_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinpr: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<p_ndims;i++) {
			if((p_dimsizes[i] != s_dimsizes[i]) || (p_dimsizes[i] != xn_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfbinpr: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
	if(tmp_p == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinpr: Unable to coerce 'p' to double");
		return(NhlFATAL);
	}

	tmp_s = (double *)coerce_input_double(s,type_s, size_x, 0, NULL, NULL);
	if(tmp_s == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinpr: Unable to coerce 's' to double");
		return(NhlFATAL);
	}

	tmp_xn = (double *)coerce_input_double(xn,type_xn, size_x, 0, NULL, NULL);
	if(tmp_xn == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinpr: Unable to coerce 'xn' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinpr: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_pr = NCL_double;
		pr = (double *) calloc(size_x, sizeof(double));
		if(pr == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfbinpr: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_pr == NCL_double)
		tmp_pr = (double *)pr;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfbinpr,DCDFBINPR)(&size_x, tmp_p, tmp_s, tmp_xn, tmp_pr, &dummy);

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


NhlErrorTypes dcdfgamp_W( void ) {
	void	*x, *shape, *scale, *p;
	int	dummy=0;
	int x_dimsizes[NCL_MAX_DIMENSIONS], shape_dimsizes[NCL_MAX_DIMENSIONS];
	int scale_dimsizes[NCL_MAX_DIMENSIONS];
	int x_ndims, shape_ndims, scale_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	shape = (void*) NclGetArgValue(
		1,
		3,
		&shape_ndims,
		shape_dimsizes,
		NULL,
		NULL,
		&type_shape,
		2);

	scale = (void*) NclGetArgValue(
		2,
		3,
		&scale_ndims,
		scale_dimsizes,
		NULL,
		NULL,
		&type_scale,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((x_ndims != shape_ndims) || (x_ndims != scale_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfgamp: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<x_ndims;i++) {
			if((x_dimsizes[i] != shape_dimsizes[i]) || (x_dimsizes[i] != scale_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfgamp: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_x = (double *)coerce_input_double(x,type_x, size_x, 0, NULL, NULL);
	if(tmp_x == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamp: Unable to coerce 'x' to double");
		return(NhlFATAL);
	}

	tmp_shape = (double *)coerce_input_double(shape,type_shape, size_x, 0, NULL, NULL);
	if(tmp_shape == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamp: Unable to coerce 'shape' to double");
		return(NhlFATAL);
	}

	tmp_scale = (double *)coerce_input_double(scale,type_scale, size_x, 0, NULL, NULL);
	if(tmp_scale == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamp: Unable to coerce 'scale' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamp: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_p = NCL_double;
		p = (double *) calloc(size_x, sizeof(double));
		if(p == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamp: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_p == NCL_double)
		tmp_p = (double *)p;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfgamp,DCDFGAMP)(&size_x, tmp_x, tmp_shape, tmp_scale, tmp_p, &dummy);

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



NhlErrorTypes dcdfgamx_W( void ) {
	void	*p, *shape, *scale, *x;
	int	dummy=0;
	int p_dimsizes[NCL_MAX_DIMENSIONS], shape_dimsizes[NCL_MAX_DIMENSIONS];
	int scale_dimsizes[NCL_MAX_DIMENSIONS];
	int p_ndims, shape_ndims, scale_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	shape = (void*) NclGetArgValue(
		1,
		3,
		&shape_ndims,
		shape_dimsizes,
		NULL,
		NULL,
		&type_shape,
		2);

	scale = (void*) NclGetArgValue(
		2,
		3,
		&scale_ndims,
		scale_dimsizes,
		NULL,
		NULL,
		&type_scale,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((p_ndims != shape_ndims) || (p_ndims != scale_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfgamx: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<p_ndims;i++) {
			if((p_dimsizes[i] != shape_dimsizes[i]) || (p_dimsizes[i] != scale_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfgamx: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
	if(tmp_p == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamx: Unable to coerce 'p' to double");
		return(NhlFATAL);
	}

	tmp_shape = (double *)coerce_input_double(shape,type_shape, size_x, 0, NULL, NULL);
	if(tmp_shape == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamx: Unable to coerce 'shape' to double");
		return(NhlFATAL);
	}

	tmp_scale = (double *)coerce_input_double(scale,type_scale, size_x, 0, NULL, NULL);
	if(tmp_scale == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamx: Unable to coerce 'scale' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamx: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_x = NCL_double;
		x = (double *) calloc(size_x, sizeof(double));
		if(x == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfgamx: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_x == NCL_double)
		tmp_x = (double *)x;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfgamx,DCDFGAMX)(&size_x, tmp_p, tmp_shape, tmp_scale, tmp_x, &dummy);

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


NhlErrorTypes dcdfnorp_W( void ) {
	void	*x, *mean, *sd, *p;
	int	dummy=0;
	int x_dimsizes[NCL_MAX_DIMENSIONS], mean_dimsizes[NCL_MAX_DIMENSIONS];
	int sd_dimsizes[NCL_MAX_DIMENSIONS];
	int x_ndims, mean_ndims, sd_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	mean = (void*) NclGetArgValue(
		1,
		3,
		&mean_ndims,
		mean_dimsizes,
		NULL,
		NULL,
		&type_mean,
		2);

	sd = (void*) NclGetArgValue(
		2,
		3,
		&sd_ndims,
		sd_dimsizes,
		NULL,
		NULL,
		&type_sd,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((x_ndims != mean_ndims) || (x_ndims != sd_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfnorp: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<x_ndims;i++) {
			if((x_dimsizes[i] != mean_dimsizes[i]) || (x_dimsizes[i] != sd_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfnorp: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_x = (double *)coerce_input_double(x,type_x, size_x, 0, NULL, NULL);
	if(tmp_x == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorp: Unable to coerce 'x' to double");
		return(NhlFATAL);
	}

	tmp_mean = (double *)coerce_input_double(mean,type_mean, size_x, 0, NULL, NULL);
	if(tmp_mean == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorp: Unable to coerce 'mean' to double");
		return(NhlFATAL);
	}

	tmp_sd = (double *)coerce_input_double(sd,type_sd, size_x, 0, NULL, NULL);
	if(tmp_sd == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorp: Unable to coerce 'sd' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorp: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_p = NCL_double;
		p = (double *) calloc(size_x, sizeof(double));
		if(p == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorp: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_p == NCL_double)
		tmp_p = (double *)p;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfnorp,DCDFNORP)(&size_x, tmp_x, tmp_mean, tmp_sd, tmp_p, &dummy);

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


NhlErrorTypes dcdfnorx_W( void ) {
	void	*p, *mean, *sd, *x;
	int	dummy=0;
	int p_dimsizes[NCL_MAX_DIMENSIONS], mean_dimsizes[NCL_MAX_DIMENSIONS];
	int sd_dimsizes[NCL_MAX_DIMENSIONS];
	int p_ndims, mean_ndims, sd_ndims;

	/* Declaring temporary variables */

	int i, size_x;
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
		2);

	mean = (void*) NclGetArgValue(
		1,
		3,
		&mean_ndims,
		mean_dimsizes,
		NULL,
		NULL,
		&type_mean,
		2);

	sd = (void*) NclGetArgValue(
		2,
		3,
		&sd_ndims,
		sd_dimsizes,
		NULL,
		NULL,
		&type_sd,
		2);


	/*
	* Make sure all of the input arguments are of equal dimensions.
	*/
	if((p_ndims != mean_ndims) || (p_ndims != sd_ndims)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfnorx: The input arrays must have the same number of dimensions");
		return(NhlFATAL);
	}
	else {
		for(i=0;i<p_ndims;i++) {
			if((p_dimsizes[i] != mean_dimsizes[i]) || (p_dimsizes[i] != sd_dimsizes[i]) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "dcdfnorx: The input arrays must have the same dimension sizes");
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
	* Coerce input arguments.
	*/
	tmp_p = (double *)coerce_input_double(p,type_p, size_x, 0, NULL, NULL);
	if(tmp_p == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorx: Unable to coerce 'p' to double");
		return(NhlFATAL);
	}

	tmp_mean = (double *)coerce_input_double(mean,type_mean, size_x, 0, NULL, NULL);
	if(tmp_mean == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorx: Unable to coerce 'mean' to double");
		return(NhlFATAL);
	}

	tmp_sd = (double *)coerce_input_double(sd,type_sd, size_x, 0, NULL, NULL);
	if(tmp_sd == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorx: Unable to coerce 'sd' to double");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorx: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}
	else {
		type_x = NCL_double;
		x = (double *) calloc(size_x, sizeof(double));
		if(x == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dcdfnorx: Unable to allocate memory for output array");
			return(NhlFATAL);
		}
	}


	if(type_x == NCL_double)
		tmp_x = (double *)x;
		
	/*
	* Call the Fortran version of this routine.
	*/
	NGCALLF(dcdfnorx,DCDFNORX)(&size_x, tmp_p, tmp_mean, tmp_sd, tmp_x, &dummy);

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

