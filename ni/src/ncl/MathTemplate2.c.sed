
/*
 *      $Id: MathTemplate2.c.sed,v 1.5 2009-02-10 18:48:09 haley Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jan 31 15:16:07 MST 1995
 *
 *	Description:	
 */


NhlErrorTypes _NclFUNCNAME
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing0,missing1,dm0,dm1;
	int has_missing0,n_dims0;
	ng_size_t dimsizes0[NCL_MAX_DIMENSIONS];
	int has_missing1,n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	double *dout_val;
	float *fout_val;
	void *value0;
	void *value1;
	double *dval0;
	double *dval1;
	float *fval0;
	float *fval1;
	NclBasicDataTypes type0,out_type;
	NclBasicDataTypes type1;
	ng_size_t total1=1;
	ng_size_t total0=1;
	ng_size_t i;

	value0 = (void*)NclGetArgValue(
			0,
			2,
			&n_dims0,
			dimsizes0,
			&missing0,
			&has_missing0,
			&type0,
			0);
	for(i = 0; i < n_dims0; i++) {
		total0 *= dimsizes0[i];
	}

	value1 = (void*)NclGetArgValue(
			1,
			2,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			&type1,
			0);
	for(i = 0; i < n_dims1; i++) {
		total1 *= dimsizes1[i];
	}
	if((type1 == NCL_double)||(type0==NCL_double)) {
		out_type = NCL_double;
	} else {
		out_type = NCL_float;
	}
	if(total0 == total1) {
		switch(out_type) {
		case NCL_double:
			if(type0 == NCL_double) {
				dval0 = (double*)value0;
				if(has_missing0) {
					dm0.doubleval = missing0.doubleval;
				}
			} else {
				dval0 = (double*)NclMalloc(sizeof(double)*total0);
				_Nclcoerce((NclTypeClass)nclTypedoubleClass,
						dval0,
						value0,
						total0,
						&missing0,
						NULL,
						(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type0))));

				dm0.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.floatval;
			}
			if(type1 == NCL_double) {
				dval1 = (double*)value1;
				if(has_missing1) {
					dm1.doubleval = missing1.doubleval;
				}
			} else {
				dval1 = (double*)NclMalloc(sizeof(double)*total1);
				_Nclcoerce((NclTypeClass)nclTypedoubleClass,
						dval1,
						value1,
						total1,
						&missing1,
						NULL,
						(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type1))));
				dm1.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
			}
		
			dout_val = (double*)NclMalloc(total0*sizeof(double));

			if(has_missing0&&!has_missing1) {
				for( i = 0 ; i < total0; i ++ ) {
					if(dval0[i] != dm0.doubleval) {
						dout_val[i] = FUNCNAME((CAST)dval0[i],(CAST)dval1[i]);
					} else {
						dout_val[i] = dm0.doubleval;
					}
				}
			} else if(has_missing1&&!has_missing0) {
				for( i = 0 ; i < total1; i ++ ) {
					if(dval1[i] != dm1.doubleval) {
						dout_val[i] = FUNCNAME((CAST)dval0[i],(CAST)dval1[i]);
					} else {
						dout_val[i] = dm1.doubleval;
					}
				}
			} else if(has_missing1&&has_missing0) {
				for( i = 0 ; i < total0; i ++ ) {
					if((dval0[i] != dm0.doubleval)&&(dval1[i] != dm1.doubleval)) {
						dout_val[i] = FUNCNAME((CAST)dval0[i],(CAST)dval1[i]);
					} else {
						dout_val[i] = dm1.doubleval;
					}
				}
			} else {
				for( i = 0 ; i < total0; i ++ ) {
					dout_val[i] = FUNCNAME((CAST)dval0[i],(CAST)dval1[i]);
				}
			}
			if((void*)dval0 != value0) {
				NclFree(dval0);
			}
			if((void*)dval1 != value1) {
				NclFree(dval1);
			}
			return(NclReturnValue(
				dout_val,
				n_dims0,
				dimsizes0,
				(has_missing0 ? &dm0 : has_missing1 ? &dm1 : NULL),
				NCL_double,
				0
			));
		case NCL_float:
			if(type0 == NCL_float) {
				fval0 = (float*)value0;
				if(has_missing0) {
					dm0.floatval = missing0.floatval;
				}
			} else {
				fval0 = (float*)NclMalloc(sizeof(float)*total0);
				_Nclcoerce((NclTypeClass)nclTypefloatClass,
						fval0,
						value0,
						total0,
						&missing0,
						NULL,
						(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type0))));
				dm0.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			}
			if(type1 == NCL_float) {
				fval1 = (float*)value1;
				if(has_missing1) {
					dm1.floatval = missing1.floatval;
				}
			} else {
				fval1 = (float*)NclMalloc(sizeof(float)*total1);
				_Nclcoerce((NclTypeClass)nclTypefloatClass,
						fval1,
						value1,
						total1,
						&missing1,
						NULL,
						(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type1))));
				dm1.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			}
		
			fout_val = (float*)NclMalloc(total0*sizeof(float));

			if(has_missing0&&!has_missing1) {
				for( i = 0 ; i < total0; i ++ ) {
					if(fval0[i] != dm0.floatval) {
						fout_val[i] = FUNCNAME((CAST)fval0[i],(CAST)fval1[i]);
					} else {
						fout_val[i] = dm0.floatval;
					}
				}
			} else if(has_missing1&&!has_missing0) {
				for( i = 0 ; i < total1; i ++ ) {
					if(fval1[i] != dm1.floatval) {
						fout_val[i] = FUNCNAME((CAST)fval0[i],(CAST)fval1[i]);
					} else {
						fout_val[i] = dm1.floatval;
					}
				}
			} else if(has_missing1&&has_missing0) {
				for( i = 0 ; i < total0; i ++ ) {
					if((fval0[i] != dm0.floatval)&&(fval1[i] != dm1.floatval)) {
						fout_val[i] = FUNCNAME((CAST)fval0[i],(CAST)fval1[i]);
					} else {
						fout_val[i] = dm1.floatval;
					}
				}
			} else {
				for( i = 0 ; i < total0; i ++ ) {
					fout_val[i] = FUNCNAME((CAST)fval0[i],(CAST)fval1[i]);
				}
			}
			if((void*)fval0 != value0) {
				NclFree(fval0);
			}
			if((void*)fval1 != value1) {
				NclFree(fval1);
			}
			return(NclReturnValue(
				fout_val,
				n_dims0,
				dimsizes0,
				(has_missing0 ? &dm0 : has_missing1 ? &dm1 : NULL),
				NCL_float,
				0
			));
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: internal error: can not continue");
			return NhlFATAL;
		}

	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME parameter 0 and parameter 1 are not the same size");
		return(NhlFATAL);
	}
}
