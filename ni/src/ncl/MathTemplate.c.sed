
/*
 *      $Id$
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
	NclScalar missing,missing2;
	int has_missing,n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	void *out_val;
	float *fout_val;
	double *dout_val;
	void *value;
	float *fvalue;
	double *dvalue;
	byte *bvalue;
	char *cvalue;
	short *svalue;
	int  *ivalue;
	long *lvalue;
	long long *llvalue;
	unsigned char *ucvalue;
	unsigned short *usvalue;
	unsigned int  *uivalue;
	unsigned long *ulvalue;
	unsigned long long *ullvalue;
	
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t i;

	value = (void*)NclGetArgValue(
			0,
			1,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);
	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	switch(type) {
	case NCL_float:
		fvalue = (float*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			for( i = 0 ; i < total; i ++ ) {
				if(fvalue[i] != missing.floatval) {
					fout_val[i] = (float)FUNCNAME((CAST)fvalue[i]);
				} else {
					fout_val[i] = (float)missing.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)fvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing : NULL),
			NCL_float,
			0
		));
	case NCL_double:
		dvalue = (double*)value;
		out_val = (void*)NclMalloc(total*sizeof(double));
		dout_val = (double*)out_val;
		if(has_missing) {
			for( i = 0 ; i < total; i ++ ) {
				if(dvalue[i] != missing.doubleval) {
					dout_val[i] = (double)FUNCNAME((CAST)dvalue[i]);
				} else {
					dout_val[i] = (double)missing.doubleval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				dout_val[i] = (double)FUNCNAME((CAST)dvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing : NULL),
			NCL_double,
			0
		));
	case NCL_byte:
		bvalue = (byte*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.byteval;
			for( i = 0 ; i < total; i ++ ) {
				if(bvalue[i] != missing.byteval) {
					fout_val[i] = (float)FUNCNAME((CAST)bvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)bvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_short:
		svalue = (short*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.shortval;
			for( i = 0 ; i < total; i ++ ) {
				if(svalue[i] != missing.shortval) {
					fout_val[i] = (float)FUNCNAME((CAST)svalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)svalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_int:
		ivalue = (int*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.intval;
			for( i = 0 ; i < total; i ++ ) {
				if(ivalue[i] != missing.intval) {
					fout_val[i] = (float)FUNCNAME((CAST)ivalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)ivalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_long:
		lvalue = (long*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.longval;
			for( i = 0 ; i < total; i ++ ) {
				if(lvalue[i] != missing.longval) {
					fout_val[i] = (float)FUNCNAME((CAST)lvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)lvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_int64:
		llvalue = (long long *)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.int64val;
			for( i = 0 ; i < total; i ++ ) {
				if(llvalue[i] != missing.int64val) {
					fout_val[i] = (float)FUNCNAME((CAST)llvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)llvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_char:
		ucvalue = (unsigned char*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.charval;
			for( i = 0 ; i < total; i ++ ) {
				if(ucvalue[i] != missing.charval) {
					fout_val[i] = (float)FUNCNAME((CAST)ucvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)ucvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_ubyte:
		ucvalue = (unsigned char *)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.ubyteval;
			for( i = 0 ; i < total; i ++ ) {
				if(ucvalue[i] != missing.ubyteval) {
					fout_val[i] = (float)FUNCNAME((CAST)ucvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)ucvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_ushort:
		usvalue = (unsigned short*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.ushortval;
			for( i = 0 ; i < total; i ++ ) {
				if(usvalue[i] != missing.ushortval) {
					fout_val[i] = (float)FUNCNAME((CAST)usvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)usvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_uint:
		uivalue = (unsigned int*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.uintval;
			for( i = 0 ; i < total; i ++ ) {
				if(uivalue[i] != missing.uintval) {
					fout_val[i] = (float)FUNCNAME((CAST)uivalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)uivalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_ulong:
		ulvalue = (unsigned long*)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.ulongval;
			for( i = 0 ; i < total; i ++ ) {
				if(ulvalue[i] != missing.ulongval) {
					fout_val[i] = (float)FUNCNAME((CAST)ulvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)ulvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	case NCL_uint64:
		ullvalue = (unsigned long long *)value;
		out_val = (void*)NclMalloc(total*sizeof(float));
		fout_val = (float*)out_val;
		if(has_missing) {
			missing2.floatval = (float)missing.uint64val;
			for( i = 0 ; i < total; i ++ ) {
				if(ullvalue[i] != missing.uint64val) {
					fout_val[i] = (float)FUNCNAME((CAST)ullvalue[i]);
				} else {
					fout_val[i] = missing2.floatval;
				}
			}
		} else {
			for( i = 0 ; i < total; i ++ ) {
				fout_val[i] = (float)FUNCNAME((CAST)ullvalue[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			n_dims,
			dimsizes,
			(has_missing ? &missing2 : NULL),
			NCL_float,
			0
		));
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME: a non-numeric type was passed to this function, can not continue");
	}
	return NhlFATAL;
}
