
/*
 *      $Id: MathTemplate.c.sed,v 1.2 1995-03-25 00:59:00 ethan Exp $
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
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	OUTDATATYPE *out_val;
	ARG0TYPE *value;
	NclBasicDataTypes type;
	int total=1;
	int i;

	value = (ARG0TYPE*)NclGetArgValue(
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

	out_val = (OUTDATATYPE*)NclMalloc(total*sizeof(OUTDATATYPE));

	if(has_missing) {
		for( i = 0 ; i < total; i ++ ) {
			if(value[i] != missing.ARG0TYPEval) {
				out_val[i] = (OUTDATATYPE)FUNCNAME((CAST)value[i]);
			} else {
				out_val[i] = (OUTDATATYPE)missing.ARG0TYPEval;
			}
		}
	} else {
		for( i = 0 ; i < total; i ++ ) {
			out_val[i] = (OUTDATATYPE)FUNCNAME((CAST)value[i]);
		}
	}

		


	return(NclReturnValue(
		out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing : NULL),
		type,
		0
	));
}
