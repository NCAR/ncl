
/*
 *      $Id: MathTemplate2.c.sed,v 1.1 1996-12-17 18:43:09 ethan Exp $
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
	NclScalar missing0,missing1,missing2;
	int has_missing0,n_dims0,dimsizes0[NCL_MAX_DIMENSIONS];
	int has_missing1,n_dims1,dimsizes1[NCL_MAX_DIMENSIONS];
	OUTDATATYPE *out_val;
	ARG0TYPE *value0;
	ARG1TYPE *value1;
	NclBasicDataTypes type0;
	NclBasicDataTypes type1;
	int total1=1;
	int total0=1;
	int i;

	value0 = (ARG0TYPE*)NclGetArgValue(
			0,
			1,
			&n_dims0,
			dimsizes0,
			&missing0,
			&has_missing0,
			&type0,
			0);
	for(i = 0; i < n_dims0; i++) {
		total0 *= dimsizes0[i];
	}

	value1 = (ARG1TYPE*)NclGetArgValue(
			0,
			1,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			&type1,
			0);
	for(i = 0; i < n_dims1; i++) {
		total1 *= dimsizes1[i];
	}
	if(total0 == total1) {

		out_val = (OUTDATATYPE*)NclMalloc(total0*sizeof(OUTDATATYPE));

		if(has_missing0&&!has_missing1) {
			for( i = 0 ; i < total0; i ++ ) {
				if(value0[i] != missing0.ARG0TYPEval) {
					out_val[i] = (OUTDATATYPE)FUNCNAME((CAST)value0[i],(CAST)value1[i]);
				} else {
					out_val[i] = (OUTDATATYPE)missing0.ARG0TYPEval;
				}
			}
		} else if(has_missing1&&!has_missing0) {
			for( i = 0 ; i < total1; i ++ ) {
				if(value1[i] != missing1.ARG1TYPEval) {
					out_val[i] = (OUTDATATYPE)FUNCNAME((CAST)value0[i],(CAST)value1[i]);
				} else {
					out_val[i] = (OUTDATATYPE)missing1.ARG1TYPEval;
				}
			}
		} else if(has_missing1&&has_missing0) {
			for( i = 0 ; i < total0; i ++ ) {
				if((value0[i] != missing1.ARG0TYPEval)&&(value1[i] != missing1.ARG1TYPEval)) {
					out_val[i] = (OUTDATATYPE)FUNCNAME((CAST)value0[i],(CAST)value1[i]);
				} else {
					out_val[i] = (OUTDATATYPE)missing1.ARG0TYPEval;
				}
			}
		} else {
			for( i = 0 ; i < total0; i ++ ) {
				out_val[i] = (OUTDATATYPE)FUNCNAME((CAST)value0[i],(CAST)value1[i]);
			}
		}

			


		return(NclReturnValue(
			out_val,
			n_dims0,
			dimsizes0,
			(has_missing0 ? &missing0 : has_missing1 ? &missing1 : NULL),
			type0,
			0
		));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FUNCNAME parameter 0 and paramter 1 are not the same size");
		return(NhlFATAL);
	}
}
